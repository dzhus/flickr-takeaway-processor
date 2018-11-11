# flickr-takeaway-processor

Embed Flickr metadata back into media files.

The following tags are populated:

| Flickr tag  | Exif/IPTC tag      |
|-------------|--------------------|
| Photo title | `Headline`         |
| Description | `ImageDescription` |
| Date taken  | `DateTimeOriginal` |

## Build & install

Use <http://haskellstack.org>:

```
stack install
```

The tool needs `exiftool` to run.

## Use

1. Unpack Flickr takeaway archives:

    - media files (`data_*_*_*.zip`) to `media/`

    - sidecar JSONs (`*_*_part*.zip`) to `meta/`

2. Run the tool

    ```
    flickr-takeway-processor media/ meta/
    ```
