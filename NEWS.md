# simqi 0.2.0

- `vcov` arguments allow for custom variance-covariance matrices (like those you would have for "robust" standard errors).
- Optimize `newdata` to spare user from having to create such a placeholder column for the DV in their prediction grid.
- Various stops/warnings added to make sure `sim_qi()` works as intended.
- Substitute for `{MASS}` for `{stevemisc}` for the purpose of simulating a multivariate normal distribution. Fewer dependencies.
- Add support for models produced by `logistf()` in the `{logistf}` package.
- Documentation updates.

# simqi 0.1.0

Initial development offering.
