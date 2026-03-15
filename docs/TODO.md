# TODO

## Frontend

- [ ] **Migrate PrintEngine modal to WindowManager** — `PrintEngine/Modal.hs` calls `modalFrame` directly, bypassing the window manager stack. `ViewerDetail.hs` manages the modal state/lifecycle manually via `Maybe PrintModalModel`. To migrate: extract into a proper `M.Component` and open via `openFramedModal`. Non-trivial due to tight coupling with ViewerDetail (shared task rendering, measurement container, post-print actions).

## Tests

- [ ] **Fix flaky P12 property test** — `P12: monotonicity — easier level mastery <= harder level mastery` in `competences-common-test` occasionally fails. Investigate whether the property is too strict or the generator produces edge cases that violate the expected ordering.
