# GitHub Actions Quick Reference

## Quick Commands

### View Workflow Status
```bash
# Via GitHub CLI
gh run list --limit 10
gh run view <run-id>
gh run watch  # Watch current run
```

### Trigger Manual Workflow
```bash
gh workflow run "CI Pipeline"
gh workflow run "Performance Tests"
gh workflow run "Nightly Build"
```

### View Logs
```bash
gh run view <run-id> --log
gh run view <run-id> --log-failed  # Only failed steps
```

## Workflow Triggers Quick Reference

| Workflow | Auto Trigger | Manual | Schedule |
|----------|--------------|--------|----------|
| ci-pipeline | ✓ (push/PR) | ✓ | - |
| elisp-lint | ✓ (push/PR) | - | - |
| elisp-compile | ✓ (push/PR) | - | - |
| elisp-test | ✓ (push/PR) | - | - |
| org-mode-validation | ✓ (push/PR) | - | - |
| javascript-ci | ✓ (push/PR) | - | - |
| security-scan | ✓ (push/PR) | - | Daily 2 AM UTC |
| documentation-check | ✓ (push/PR) | - | - |
| code-quality | ✓ (push/PR) | - | - |
| dependency-updates | - | ✓ | Mon 9 AM UTC |
| nightly-build | - | ✓ | Daily 2 AM UTC |
| performance-tests | ✓ (push/PR) | ✓ | Sun 12 AM UTC |
| release | ✓ (tags) | ✓ | - |
| test (legacy) | ✓ (push/PR) | - | - |

## File Patterns That Trigger Workflows

### Elisp Workflows
- `*.el` → elisp-lint, elisp-compile, elisp-test
- `test/**` → elisp-test
- `Cask` → elisp-compile, elisp-test

### Org Workflows
- `*.org` → org-mode-validation, documentation-check

### JavaScript Workflows
- `src/**` → javascript-ci
- `package.json`, `package-lock.json` → javascript-ci
- `vite.config.js`, `tailwind.config.js` → javascript-ci

### Documentation Workflows
- `README*`, `CHANGELOG*` → documentation-check
- `demos/**` → documentation-check

## Common Workflow Commands

### Local Testing

```bash
# Validate YAML
python3 -c "import yaml; yaml.safe_load(open('.github/workflows/ci-pipeline.yml'))"

# Test Elisp
cask install
cask emacs --batch -f batch-byte-compile *.el
cask exec ert-runner

# Test JavaScript
npm ci
npm run build
npm test

# Test Org
cask emacs --batch --eval "(require 'org)" --eval "(find-file \"README.org\")"
```

## Status Badge URLs

```markdown
![CI](https://github.com/drzo/emacs-aichat-skintwin/workflows/CI%20Pipeline/badge.svg)
![Lint](https://github.com/drzo/emacs-aichat-skintwin/workflows/Elisp%20Lint/badge.svg)
![Compile](https://github.com/drzo/emacs-aichat-skintwin/workflows/Elisp%20Compile/badge.svg)
![Test](https://github.com/drzo/emacs-aichat-skintwin/workflows/Elisp%20Test/badge.svg)
![Security](https://github.com/drzo/emacs-aichat-skintwin/workflows/Security%20Scan/badge.svg)
```

## Troubleshooting Quick Fixes

### Cask Issues
```bash
# Clear cache and reinstall
rm -rf .cask
cask install
```

### NPM Issues
```bash
# Clear cache and reinstall
rm -rf node_modules package-lock.json
npm install
```

### Test Failures
```bash
# Run specific test
cask emacs --batch -L . -l test/aichat-util-test.el -f ert-run-tests-batch-and-exit
```

### Build Issues
```bash
# Clean build
rm -rf dist
npm run build
```

## Workflow File Locations

```
.github/workflows/
├── README.md              # Full documentation
├── TESTING.md             # Testing guide
├── ci-pipeline.yml        # Main orchestration
├── elisp-lint.yml         # Elisp linting
├── elisp-compile.yml      # Byte compilation
├── elisp-test.yml         # Unit tests
├── org-mode-validation.yml # Org validation
├── javascript-ci.yml      # JS build/test
├── security-scan.yml      # Security checks
├── documentation-check.yml # Doc validation
├── code-quality.yml       # Quality metrics
├── dependency-updates.yml # Dep management
├── nightly-build.yml      # Nightly builds
├── performance-tests.yml  # Benchmarks
├── release.yml            # Releases
└── test.yml               # Legacy tests
```

## Matrix Configurations

### Emacs Versions
- 27.2 (older LTS)
- 28.2 (previous stable)
- 29.1 (current stable)

### Node.js Versions
- 18.x (older LTS)
- 20.x (current LTS)

### Operating Systems
- ubuntu-latest (primary)
- macos-latest (nightly only)

## Artifact Retention

| Type | Days | Workflow |
|------|------|----------|
| Compiled Elisp | 7 | elisp-compile |
| Test Results | 30 | elisp-test |
| Build Artifacts | 7 | javascript-ci |
| Coverage | 30 | elisp-test, javascript-ci |
| HTML Exports | 7 | org-mode-validation |
| Reports | 30 | documentation-check, code-quality |
| Performance | 90 | performance-tests |
| Nightly Builds | 7 | nightly-build |

## Emergency Actions

### Disable a Workflow
1. Go to Actions tab
2. Select workflow
3. Click "..." menu
4. Select "Disable workflow"

### Cancel Running Workflows
```bash
gh run cancel <run-id>
# Or via UI: Actions → Run → Cancel workflow
```

### Re-run Failed Jobs
```bash
gh run rerun <run-id>
gh run rerun <run-id> --failed  # Only failed jobs
```

## Best Practices Checklist

- [ ] Test locally before pushing
- [ ] Use descriptive commit messages
- [ ] Check workflow status after push
- [ ] Review logs if workflows fail
- [ ] Keep workflows up to date
- [ ] Monitor scheduled workflows
- [ ] Clean up old artifacts
- [ ] Document workflow changes

## Need Help?

1. Check [TESTING.md](TESTING.md) for detailed testing guide
2. Check [README.md](README.md) for full documentation
3. Review workflow logs
4. Search existing issues
5. Create new issue with details
