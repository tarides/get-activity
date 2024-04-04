## unreleased

### Fixed

- Filter out null elements that are sometimes in the nodes list (#<PR_NUMBER>, @gpetiot)

## 2.0.0

### Fixed

- Take into account the end date of the specified period when filtering github activity (#31, @gpetiot)

### Changed

- API: `Contributions.of_json` parameter `~from` is replaced by `~period` (#31, @gpetiot)
- Distinguish between issue comments and PR comments (#38, @gpetiot)
  API: new constructor `Comment` replacing `Issue_comment`

### Added

- Display curl requests and responses in debug mode (`-vv` or `--verbosity debug`) (#36, @gpetiot)
- Add the PR merge events to the contributions (#37, @emillon, @gpetiot)
  API: new constructor `Merge`

## 1.0.1

### Fixed

- `ppx_expect` is now only used in tests (#29, @gpetiot)

## 1.0.0

### Added

- Add the `--version` command-line option (#13, @gpetiot)
- Add a `--user` option to extract the activity of an engineer that is not the current user (#14, @gpetiot)
- Add the issue comments to the contributions (#21, @gpetiot)

### Changed

- Replace exceptions by result types for the requests (#11, @gpetiot)
- Depends on `curly` instead of `cohttp-lwt-unix` (#12, @gpetiot)
- Redesign the graphql requests (#12, @gpetiot)
  + `Graphql.exec` now takes a `request`
  + `Contributions.fetch` has been replaced by `Contributions.request` that builds a `request`
- Add a `~user:User.t` parameter to `Contributions.request` and `Contributions.of_json` (#14, @gpetiot)
- `Contributions.of_json` now returns a result type (#20, @gpetiot)

## 0.2.0

### Added

Expose the library `get-activity-lib` as an opam package (#4, @gpetiot)
- Expose `Get_ctivity.Period`
- Expose `Get_ativity.Contributions.Datetime`
- Expose `Get_activity.Contributions.Repo_map`
- Expose `Get_activity.Contributions.item`
- Add a `username` field to `Get_activity.Contributions.t`
- Label the parameters of `Get_activity.Graphql.exec`

## 0.1

(changes before Feb '24 not tracked)
