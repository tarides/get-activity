## unreleased

### Added

- Add the `--version` command-line option (#13, @gpetiot)
- Add a `--user` option to extract the activity of an engineer that is not the current user (#14, @gpetiot)

### Changed

- Replace exceptions by result types for the requests (#11, @gpetiot)
- Depends on `curly` instead of `cohttp-lwt-unix` (#12, @gpetiot)
- Redesign the graphql requests (#12, @gpetiot)
  + `Graphql.exec` now takes a `request`
  + `Contributions.fetch` has been replaced by `Contributions.request` that builds a `request`
- Add a `~user:User.t` parameter to `Contributions.request` and `Contributions.of_json` (#14, @gpetiot)

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
