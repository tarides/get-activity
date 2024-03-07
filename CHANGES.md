## unreleased

### Changed

- Replace exceptions by result types for the requests (#11, @gpetiot)
  + `Graphql.exec` now returns `_ result Lwt.t`
  + `Contributions.fetch` now returns `_ result`

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
