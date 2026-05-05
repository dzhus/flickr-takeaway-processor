#!/usr/bin/env python3
"""
Process Flickr takeaway archives: organize by first album, embed metadata with exiftool,
mark non-public files with a .private extension segment.
"""

from __future__ import annotations

import argparse
import json
import logging
import shutil
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass
from datetime import datetime
from fractions import Fraction
from pathlib import Path
from typing import Any, Iterable

# ---------------------------------------------------------------------------
# Data model
# ---------------------------------------------------------------------------


@dataclass
class Geo:
    latitude: float
    longitude: float


@dataclass
class Tag:
    text: str


class Privacy:
    PUBLIC = "public"
    PRIVATE = "private"
    FRIENDS_AND_FAMILY = "friend & family"


@dataclass
class PhotoMeta:
    id: str
    date_taken: datetime
    description: str
    name: str
    geo: list[Geo]
    albums: list[dict[str, Any]]
    tags: list[Tag]
    privacy: str


def _parse_rational_div_1e6(raw: str | int | float) -> float:
    if isinstance(raw, (int, float)):
        return float(raw) / 1e6
    s = str(raw).strip()
    if "/" in s:
        num_s, den_s = s.split("/", 1)
        return float(Fraction(int(num_s.strip()), int(den_s.strip()))) / 1e6
    return float(Fraction(s)) / 1e6


def _parse_geo(obj: dict[str, Any]) -> Geo:
    return Geo(
        latitude=_parse_rational_div_1e6(obj["latitude"]),
        longitude=_parse_rational_div_1e6(obj["longitude"]),
    )


def _parse_local_time(s: str) -> datetime:
    s = s.strip()
    for fmt in ("%Y-%m-%d %H:%M:%S", "%Y-%m-%dT%H:%M:%S", "%Y-%m-%d"):
        try:
            return datetime.strptime(s, fmt)
        except ValueError:
            continue
    raise ValueError(f"Unrecognized date_taken format: {s!r}")


def _parse_privacy(raw: str) -> str:
    key = raw.strip().lower()
    if key == Privacy.PUBLIC:
        return Privacy.PUBLIC
    if key == Privacy.PRIVATE:
        return Privacy.PRIVATE
    if key == Privacy.FRIENDS_AND_FAMILY:
        return Privacy.FRIENDS_AND_FAMILY
    raise ValueError(f"Unexpected privacy value: {raw!r}")


def parse_sidecar(data: dict[str, Any]) -> PhotoMeta:
    tags_raw = data.get("tags") or []
    tags = []
    for t in tags_raw:
        if isinstance(t, dict) and "tag" in t:
            tags.append(Tag(text=str(t["tag"])))
        else:
            raise ValueError(f"Invalid tag entry: {t!r}")

    geo_raw = data.get("geo") or []
    geo = [_parse_geo(g) for g in geo_raw]

    albums = list(data.get("albums") or [])

    return PhotoMeta(
        id=str(data["id"]),
        date_taken=_parse_local_time(str(data["date_taken"])),
        description=str(data.get("description", "")),
        name=str(data.get("name", "")),
        geo=geo,
        albums=albums,
        tags=tags,
        privacy=_parse_privacy(str(data["privacy"])),
    )


def album_label(album_obj: dict[str, Any]) -> str | None:
    title = album_obj.get("title")
    if title:
        return str(title).strip() or None
    aid = album_obj.get("id")
    if aid:
        return str(aid).strip() or None
    url = album_obj.get("url") or ""
    if isinstance(url, str) and url.strip():
        seg = url.rstrip("/").split("/")[-1]
        return seg.strip() or None
    return None


def first_album_folder_name(albums: list[dict[str, Any]]) -> str | None:
    for a in albums:
        if isinstance(a, dict):
            lab = album_label(a)
            if lab:
                return lab
    return None


def is_non_public(privacy: str) -> bool:
    return privacy in (Privacy.PRIVATE, Privacy.FRIENDS_AND_FAMILY)


# ---------------------------------------------------------------------------
# Path logic (mirrors Main.hs)
# ---------------------------------------------------------------------------


def list_media_depth_1_2(media_dir: Path) -> list[Path]:
    out: list[Path] = []
    if not media_dir.is_dir():
        return out
    for p in media_dir.iterdir():
        if p.is_file():
            out.append(p)
        elif p.is_dir():
            for q in p.iterdir():
                if q.is_file():
                    out.append(q)
    return out


def find_file(media_files: Iterable[Path], pm: PhotoMeta) -> Path | None:
    needle_o = f"{pm.id}_o"
    for p in media_files:
        if needle_o in p.name:
            return p
    for p in media_files:
        if pm.id in p.name:
            return p
    return None


def fixed_filename(path: Path) -> Path:
    name = path.name
    if name.startswith("-"):
        return path.with_name("photo" + name)
    return path


def album_filename(pm: PhotoMeta, path: Path) -> Path:
    label = first_album_folder_name(pm.albums)
    if not label:
        return path
    if label in path.parts:
        return path
    return path.parent / label / path.name


def private_filename(pm: PhotoMeta, path: Path) -> Path:
    if not is_non_public(pm.privacy):
        return path
    parts = path.name.split(".")
    if len(parts) < 2:
        return path
    if "private" in parts[:-1]:
        return path
    new_name = ".".join(parts[:-1] + ["private", parts[-1]])
    return path.with_name(new_name)


def target_path(pm: PhotoMeta, path: Path) -> Path:
    return private_filename(pm, album_filename(pm, fixed_filename(path)))


def maybe_rename(pm: PhotoMeta, path: Path, log: logging.Logger) -> bool:
    dest = target_path(pm, path)
    if dest == path:
        return False
    dest.parent.mkdir(parents=True, exist_ok=True)
    log.debug("Moving %s to %s", path, dest)
    shutil.move(str(path), str(dest))
    return True


# ---------------------------------------------------------------------------
# Exiftool
# ---------------------------------------------------------------------------


def _timestamp(dt: datetime, with_time: bool) -> str:
    if with_time:
        return dt.strftime("%Y-%m-%dT%H:%M:%S")
    return dt.strftime("%Y-%m-%d")


def _signum_show(x: float) -> str:
    return str(float((x > 0) - (x < 0)))


def make_exiftool_tags(pm: PhotoMeta) -> list[tuple[str, str]]:
    desc = pm.description.strip()
    title = pm.name.strip()
    dt = pm.date_taken
    rows: list[tuple[str, str]] = [
        ("ImageDescription", desc),
        ("xmp-dc:ImageDescription", desc),
        ("Headline", title),
        ("xmp-dc:Title", title),
        ("DateTimeOriginal", _timestamp(dt, True)),
        ("xmp-exif:DateTimeOriginal", _timestamp(dt, True)),
        ("xmp-dc:Date", _timestamp(dt, False)),
        ("CodedCharacterSet", "UTF-8"),
    ]
    for t in pm.tags:
        rows.append(("keywords", t.text))
    if pm.geo:
        g = pm.geo[0]
        lat_s = str(g.latitude)
        lon_s = str(g.longitude)
        rows.extend(
            [
                ("GPSLatitudeRef", _signum_show(g.latitude)),
                ("GPSLatitude", lat_s),
                ("xmp-exif:GPSLatitude", lat_s),
                ("GPSLongitudeRef", _signum_show(g.longitude)),
                ("GPSLongitude", lon_s),
                ("xmp-exif:GPSLongitude", lon_s),
            ]
        )
    return rows


EXIFTOOL_OPTIONS = [
    "-charset",
    "exif=UTF-8",
    "-charset",
    "iptc=UTF-8",
    "-overwrite_original_in_place",
    "-ignoreMinorErrors",
]


def format_exiftool_args(tags: list[tuple[str, str]], file_path: Path) -> list[str]:
    args = [f"-{k}={v}" for k, v in tags]
    args.extend(EXIFTOOL_OPTIONS)
    args.append(str(file_path))
    return args


def run_exiftool(tags: list[tuple[str, str]], file_path: Path) -> tuple[bool, str]:
    cmd = ["exiftool", *format_exiftool_args(tags, file_path)]
    proc = subprocess.run(
        cmd,
        capture_output=True,
        text=True,
        check=False,
    )
    err = (proc.stderr or "").strip()
    ok = proc.returncode == 0
    return ok, err


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------


def discover_sidecars(meta_dir: Path) -> list[Path]:
    if not meta_dir.is_dir():
        return []
    return sorted(meta_dir.rglob("photo_*.json"))


def parse_sidecar_file(path: Path) -> tuple[Path, PhotoMeta | None, str | None]:
    try:
        raw = path.read_bytes()
        data = json.loads(raw.decode("utf-8"))
        if not isinstance(data, dict):
            return path, None, "Sidecar root must be a JSON object"
        return path, parse_sidecar(data), None
    except (OSError, UnicodeDecodeError, json.JSONDecodeError, ValueError, KeyError) as e:
        return path, None, str(e)


def main() -> int:
    parser = argparse.ArgumentParser(description="Process Flickr takeaway files")
    parser.add_argument("media", type=Path, help="Directory with media files")
    parser.add_argument("meta", type=Path, help="Directory containing photo_*.json files")
    parser.add_argument(
        "-t",
        "--threads",
        type=int,
        default=10,
        metavar="N",
        help="Maximum number of threads for I/O-bound operations (default: 10)",
    )
    args = parser.parse_args()
    media_dir = args.media.expanduser().resolve()
    meta_dir = args.meta.expanduser().resolve()
    max_workers = max(1, args.threads)

    logging.basicConfig(level=logging.INFO, format="%(levelname)s: %(message)s")
    log = logging.getLogger(__name__)

    json_paths = discover_sidecars(meta_dir)
    parsed: list[tuple[Path, PhotoMeta]] = []
    failed_parse: list[tuple[Path, str]] = []

    def worker_parse(jp: Path) -> tuple[Path, PhotoMeta | None, str | None]:
        return parse_sidecar_file(jp)

    with ThreadPoolExecutor(max_workers=max_workers) as ex:
        futs = {ex.submit(worker_parse, jp): jp for jp in json_paths}
        for fut in as_completed(futs):
            path, meta, err = fut.result()
            if meta is not None:
                parsed.append((path, meta))
            else:
                failed_parse.append((path, err or "unknown error"))

    parsed.sort(key=lambda x: str(x[0]))
    failed_parse.sort(key=lambda x: str(x[0]))

    log.info("%s/%s sidecar .json files parsed", len(parsed), len(json_paths))
    if failed_parse:
        log.error("Could not parse %s sidecar files", len(failed_parse))
        for sidecar_path, msg in failed_parse:
            log.error("Error parsing %s: %s", sidecar_path, msg)

    original_media = list_media_depth_1_2(media_dir)
    move_count = 0

    def worker_rename(item: tuple[Path, PhotoMeta]) -> bool:
        _sc, pm = item
        path = find_file(original_media, pm)
        if path is None:
            return False
        return maybe_rename(pm, path, log)

    with ThreadPoolExecutor(max_workers=max_workers) as ex:
        futs = [ex.submit(worker_rename, item) for item in parsed]
        for fut in as_completed(futs):
            if fut.result():
                move_count += 1

    if move_count > 0:
        log.info("Renamed %s files", move_count)

    media_files = list_media_depth_1_2(media_dir)
    tasks: list[tuple[Path, PhotoMeta]] = []
    unmapped: list[Path] = []

    for sc_path, pm in parsed:
        f = find_file(media_files, pm)
        if f is None:
            unmapped.append(sc_path)
        else:
            tasks.append((f, pm))

    if unmapped:
        log.error("Media files not found for %s sidecars", len(unmapped))
        for sidecar_path in unmapped:
            log.error("No media file found for %s", sidecar_path)

    log.info("Writing tags for %s files", len(tasks))

    errors: list[tuple[Path, str]] = []
    oks = 0

    def worker_exif(item: tuple[Path, PhotoMeta]) -> tuple[Path, bool, str]:
        fpath, pm = item
        tags = make_exiftool_tags(pm)
        ok, err = run_exiftool(tags, fpath)
        return fpath, ok, err

    with ThreadPoolExecutor(max_workers=max_workers) as ex:
        futs = {ex.submit(worker_exif, item): item[0] for item in tasks}
        for fut in as_completed(futs):
            fpath, ok, err = fut.result()
            if ok:
                oks += 1
                if err and "warning" in err.lower():
                    log.warning("Warnings for %s: %s", fpath, err)
            else:
                errors.append((fpath, err))
                log.error("Errors for %s: %s", fpath, err)

    log.info("Exiftool successfully ran for %s files", oks)
    if errors:
        log.error("%s errors occurred", len(errors))

    return 1 if (failed_parse or unmapped or errors) else 0


if __name__ == "__main__":
    sys.exit(main())
