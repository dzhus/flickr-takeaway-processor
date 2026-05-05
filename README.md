# flickr-takeaway-processor

Flickr allows you to [request][data-request] all your photos with the
associated metadata such as geotags and photo descriptions. This
metadata is normally populated via Flickr's web UI, and Flickr
takeaway archive includes metadata in form of «sidecar» JSON files
with vendor-specific schema:

```json
{
    "id": "23425673160",
    "name": "London Santacon 2015",
    "description": "Look at all these lovely people",
    "date_taken": "2015-12-12 13:11:14",
    "date_imported": "2015-12-13 05:57:41",
    "privacy": "public",
    "albums": [
        {
            "title": "London Santacon 2015",
            "url": "https://www.flickr.com/photos/nothingpersonal/sets/72157654596017442/",
            "id": "72157654596017442"
        }
    ],
    "tags": [
        {
            "tag": "Santacon"
        }
    ]
}
```

If you plan to move off Flickr, you may prefer a more portable format
for this data. `flickr-takeaway-processor` does just that.

## What does it do

### Organize photo files by albums

For each of your albums a folder is created with all photos from that
album being moved there. If a photo is in several albums, only the
first one is used.

### Embed metadata

Flickr metadata for each photo is embedded back into your media file
EXIF tags.

The following tags are populated:

| Flickr metadata | [Exif/IPTC/XMP][tags] tag                     |
|-----------------|-----------------------------------------------|
| Photo title     | `Headline`, `xmp-dc:Title`                    |
| Description     | `ImageDescription`, `xmp-dc:ImageDescription` |
| Date taken      | `DateTimeOriginal`                            |
| Geotag          | `GPSLatitude`, `GPSLongitude`                 |
| Tags            | `Keywords`                                    |

### Rename private files

Files with non-public privacy level on Flickr will have `.private`
suffix added to their extension list, as in:

```
media/img_20150328_152050_22212850691_o.jpg → media/img_20150328_152050_22212850691_o.private.jpg
```

## Requirements

- Python 3.9 or newer (stdlib only; no extra packages)
- [`exiftool`][exiftool] on your `PATH`

## Use

1. Request your Flickr data from the [account settings
   page][data-request].

2. Unpack Flickr takeaway archives:

    - media files (`data_*_*_*.zip`) to `media/`

    - sidecar JSONs (`*_*_part*.zip`) to `meta/`

3. Run the tool:

    ```
    python3 flickr_takeaway_processor.py media/ meta/
    ```

    Optional: limit concurrency with `-t` / `--threads` (default is 10), for example:

    ```
    python3 flickr_takeaway_processor.py media/ meta/ -t 4
    ```

    You can also mark the script executable (`chmod +x flickr_takeaway_processor.py`) and run `./flickr_takeaway_processor.py media/ meta/`.

[exiftool]: https://sno.phy.queensu.ca/~phil/exiftool/
[tags]: https://sno.phy.queensu.ca/~phil/exiftool/TagNames/
[data-request]: https://www.flickr.com/account
