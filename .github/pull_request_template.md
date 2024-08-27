Pull request requirements:

- [ ] Commits are well written and well organized.
- [ ] Commits for a specific implementation should be prefixed with
  the implementation name.
- [ ] Github Actions CI passes all checks (including self-host)

Additional requirements if you are adding a new implementation (see [FAQ](../docs/FAQ.md#add_implementation) for details):

- [ ] Follow incremental structure (no common eval code)
- [ ] Add `impls/<IMPL>/Dockerfile`
- [ ] Add `impls/<IMPL>/Makefile`
- [ ] Update `IMPLS.yml`
- [ ] Update `Makefile.impls`
- [ ] Update `README.md`
