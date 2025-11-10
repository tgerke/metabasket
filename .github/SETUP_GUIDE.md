# GitHub Actions Setup Guide

## What's Been Created

Three GitHub Actions workflows have been set up:

1. **R-CMD-check.yaml** - Runs `R CMD check` on multiple OS/R versions
2. **test-coverage.yaml** - Measures test coverage and reports to Codecov
3. **pkgdown.yaml** - Builds and deploys your pkgdown site to GitHub Pages

## Required Setup Steps

### 1. Enable GitHub Pages

1. Go to your repository on GitHub: https://github.com/tgerke/metabasket
2. Click **Settings** → **Pages** (in left sidebar)
3. Under "Source", select:
   - Branch: `gh-pages`
   - Folder: `/ (root)`
4. Click **Save**

The site will be available at: https://tgerke.github.io/metabasket

### 2. Enable Codecov (Optional but Recommended)

For test coverage reporting:

1. Go to https://codecov.io/ and sign in with GitHub
2. Add the `tgerke/metabasket` repository
3. You'll get a token (CODECOV_TOKEN)
4. Go to your GitHub repo: **Settings** → **Secrets and variables** → **Actions**
5. Click **New repository secret**
   - Name: `CODECOV_TOKEN`
   - Value: (paste the token from Codecov)
6. Click **Add secret**

**Note:** The workflow will run without this, but coverage reports won't be uploaded to Codecov.

### 3. Update README.md

Re-knit your README.Rmd to include the new badges:

```r
rmarkdown::render("README.Rmd")
```

Or in RStudio: Open README.Rmd and click **Knit**

### 4. Commit and Push

```bash
git add .github/ README.Rmd README.md
git commit -m "Add GitHub Actions workflows for CI/CD"
git push
```

## What Happens After Push

1. **R-CMD-check** will run on macOS, Windows, and Ubuntu with multiple R versions
2. **test-coverage** will run tests and measure coverage
3. **pkgdown** will build your documentation site and deploy to GitHub Pages

You can monitor the workflows at: https://github.com/tgerke/metabasket/actions

## Badges in README

Three badges have been added:

- **R-CMD-check**: Shows if package passes checks
- **Codecov**: Shows test coverage percentage
- **Lifecycle**: Indicates package maturity (experimental)

These will automatically update as workflows run.

## Troubleshooting

If workflows fail:

1. Check the Actions tab on GitHub for error messages
2. Common issues:
   - Missing system dependencies (usually handled by r-lib/actions)
   - Test failures (fix tests locally first)
   - Suggested packages not installed (add to DESCRIPTION if needed)

## Making Changes

To update workflows later, edit the YAML files in `.github/workflows/` and commit changes.
