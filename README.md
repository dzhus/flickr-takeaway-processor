# flickr-takeaway-processor

Embed Flickr metadata back into your media file EXIF tags.

The following tags are populated:

| Flickr metadata | [Exif/IPTC/XMP][tags] tag                     |
|-----------------|-----------------------------------------------|
| Photo title     | `Headline`, `xmp-dc:Title`                    |
| Description     | `ImageDescription`, `xmp-dc:ImageDescription` |
| Date taken      | `DateTimeOriginal`                            |
| Geotag          | `GPSLatitude`, `GPSLongitude`                 |
| Tags            | `Keywords`                                    |

## Build & install

Use <http://haskellstack.org>:

```
stack install
```

`flickr-takeaway-processor` also requires [`exiftool`][exiftool] to
run.

## Use

1. Request your Flickr data from the [account settings
   page](https://www.flickr.com/account).

2. Unpack Flickr takeaway archives:

    - media files (`data_*_*_*.zip`) to `media/`

    - sidecar JSONs (`*_*_part*.zip`) to `meta/`

2. Run the tool

    ```
    flickr-takeway-processor media/ meta/
    ```

[exiftool]: https://sno.phy.queensu.ca/~phil/exiftool/
[tags]: https://sno.phy.queensu.ca/~phil/exiftool/TagNames/
