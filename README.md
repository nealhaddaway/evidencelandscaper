# evidencelandscaper

An R package for reproducible local evidence-map projects. Under development.

## Map storage

The package code and all review data are separate. Choose an existing local
folder (for example, a **Google Drive for desktop** synchronised directory)
once on each computer:

```r
evidencelandscaper::set_map_root("~/Google Drive/EvidenceMaps")
evidencelandscaper::map_root()
```

Use the **actual path on your computer**. The package does not access Google
Drive's cloud API. Avoid working on files that are not available offline, and
allow syncing to finish before using the same map on another computer.

Create and open maps:

```r
map <- evidencelandscaper::create_map("almeria_greenhouses")
evidencelandscaper::open_map("almeria_greenhouses")
```

The chosen root is stored in the user's machine-specific R configuration,
not in this Git repository or the installed R package. It may be changed
with `set_map_root()`. Each map contains its own `map.yml`, inputs,
data, runs and outputs. Local outputs are not committed to this repository.
Zenodo archival will be introduced later for one curated final record per map.

## W00 migration

W00 will be ported from the **GitHub Actions execution pathway** on
`thesalmonandthetomato/LivingEvidenceMap`, branch `main`.
The upstream source is read-only. Source selection, query translation,
per-source retrieval, checksum validation, provenance and W01 compatibility
must be retained. The package does **not yet run W00**.
