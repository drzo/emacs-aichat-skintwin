# GitHub Actions Workflows Documentation

This repository uses a comprehensive suite of GitHub Actions workflows to ensure code quality, compilation, testing, and security.

## Workflows Overview

### Core CI Workflows

#### 1. **CI Pipeline** (`ci-pipeline.yml`)
- **Trigger**: Push to main/develop branches, pull requests
- **Purpose**: Orchestrates all CI checks in a coordinated manner
- **Jobs**: 
  - Pre-checks (merge conflicts, file permissions, large files)
  - Coordinates all other workflow runs
  - Provides final CI summary
- **Badge**: ![CI Pipeline](https://github.com/drzo/emacs-aichat-skintwin/workflows/CI%20Pipeline/badge.svg)

#### 2. **Elisp Lint** (`elisp-lint.yml`)
- **Trigger**: Changes to `.el` files
- **Purpose**: Code quality checks for Emacs Lisp
- **Tests**:
  - `checkdoc` - Documentation linting
  - `package-lint` - Package structure validation
  - Trailing whitespace detection
  - Tab character detection
  - File encoding verification (UTF-8)
- **Matrix**: Emacs 27.2, 28.2, 29.1
- **Badge**: ![Elisp Lint](https://github.com/drzo/emacs-aichat-skintwin/workflows/Elisp%20Lint/badge.svg)

#### 3. **Elisp Compile** (`elisp-compile.yml`)
- **Trigger**: Changes to `.el` files or Cask file
- **Purpose**: Byte-compilation verification
- **Tests**:
  - Byte-compile all Elisp files
  - Check for compilation warnings
  - Load test for main modules
- **Matrix**: Emacs 27.2, 28.2, 29.1
- **Artifacts**: Compiled `.elc` files
- **Badge**: ![Elisp Compile](https://github.com/drzo/emacs-aichat-skintwin/workflows/Elisp%20Compile/badge.svg)

#### 4. **Elisp Test** (`elisp-test.yml`)
- **Trigger**: Changes to `.el` files or test files
- **Purpose**: Run all unit and integration tests
- **Tests**:
  - ERT runner tests
  - Individual test files
  - OpenCog module tests
  - SkinTwin module tests
  - Test coverage reporting
  - Org-mode integration tests
  - Full system initialization
- **Matrix**: Emacs 27.2, 28.2, 29.1
- **Artifacts**: Test results and coverage reports
- **Badge**: ![Elisp Test](https://github.com/drzo/emacs-aichat-skintwin/workflows/Elisp%20Test/badge.svg)

#### 5. **Org-mode Validation** (`org-mode-validation.yml`)
- **Trigger**: Changes to `.org` files
- **Purpose**: Validate Org-mode files
- **Tests**:
  - Syntax validation
  - Structure checking (heading depth)
  - HTML export verification
  - Broken internal links detection
  - Code block validation
  - SkinTwin knowledge base validation
- **Artifacts**: HTML exports
- **Badge**: ![Org-mode Validation](https://github.com/drzo/emacs-aichat-skintwin/workflows/Org-mode%20Validation/badge.svg)

#### 6. **JavaScript CI** (`javascript-ci.yml`)
- **Trigger**: Changes to `src/`, `package.json`, or config files
- **Purpose**: Build and test JavaScript/React components
- **Tests**:
  - Linting (if configured)
  - Vite build
  - Bundle verification
  - Unit tests with Vitest
  - Test coverage
  - Dependency audit
  - Outdated package check
- **Matrix**: Node.js 18.x, 20.x
- **Artifacts**: Build artifacts, coverage reports
- **Badge**: ![JavaScript CI](https://github.com/drzo/emacs-aichat-skintwin/workflows/JavaScript%20CI/badge.svg)

### Quality Workflows

#### 7. **Code Quality** (`code-quality.yml`)
- **Trigger**: Push or pull request
- **Purpose**: Comprehensive code quality analysis
- **Checks**:
  - Complexity analysis (function counts)
  - Code statistics (lines of code)
  - Long function detection (>50 lines)
  - Code duplication detection
  - Indentation checking
  - Line length validation (80 chars)
  - Naming convention verification
  - Dependency analysis (unused requires, circular deps)
  - Test coverage estimation
- **Artifacts**: Quality report
- **Badge**: ![Code Quality](https://github.com/drzo/emacs-aichat-skintwin/workflows/Code%20Quality/badge.svg)

#### 8. **Documentation Check** (`documentation-check.yml`)
- **Trigger**: Changes to documentation files
- **Purpose**: Ensure documentation quality
- **Checks**:
  - README and CHANGELOG existence
  - Markdown linting
  - Link validation
  - Demo files verification
  - Code examples extraction
  - TOC generation
  - Version consistency
  - HTML export
  - TODO/FIXME comments
  - File header verification
  - Spell checking
- **Artifacts**: Generated documentation HTML
- **Badge**: ![Documentation Check](https://github.com/drzo/emacs-aichat-skintwin/workflows/Documentation%20Check/badge.svg)

### Security Workflows

#### 9. **Security Scan** (`security-scan.yml`)
- **Trigger**: Push, pull request, daily schedule (2 AM UTC)
- **Purpose**: Security vulnerability detection
- **Scans**:
  - CodeQL analysis (JavaScript)
  - Dependency review (on PRs)
  - Secret scanning (TruffleHog)
  - NPM security audit
  - Elisp security patterns (eval, shell-command)
  - Hardcoded credentials detection
- **Artifacts**: Audit reports
- **Schedule**: Daily at 2 AM UTC
- **Badge**: ![Security Scan](https://github.com/drzo/emacs-aichat-skintwin/workflows/Security%20Scan/badge.svg)

### Maintenance Workflows

#### 10. **Dependency Updates** (`dependency-updates.yml`)
- **Trigger**: Weekly schedule (Monday 9 AM UTC), manual
- **Purpose**: Keep dependencies up to date
- **Actions**:
  - NPM dependency updates
  - Cask dependency updates
  - Auto-creates pull requests
  - Dependabot PR auto-merge (when configured)
- **Schedule**: Weekly on Monday at 9 AM UTC
- **Badge**: ![Dependency Updates](https://github.com/drzo/emacs-aichat-skintwin/workflows/Dependency%20Updates/badge.svg)

#### 11. **Nightly Build** (`nightly-build.yml`)
- **Trigger**: Daily schedule (2 AM UTC), manual
- **Purpose**: Continuous integration testing
- **Jobs**:
  - Full system build
  - All tests execution
  - Multi-platform testing (Ubuntu, macOS)
  - Multi-version testing (Emacs 27.2, 28.2, 29.1)
  - Nightly package generation
- **Artifacts**: Nightly builds, test reports
- **Schedule**: Daily at 2 AM UTC
- **Badge**: ![Nightly Build](https://github.com/drzo/emacs-aichat-skintwin/workflows/Nightly%20Build/badge.svg)

#### 12. **Performance Tests** (`performance-tests.yml`)
- **Trigger**: Push to main/develop, pull request, weekly schedule, manual
- **Purpose**: Performance benchmarking and regression detection
- **Benchmarks**:
  - AtomSpace operations (100 iterations)
  - ECAN spreading (10 iterations)
  - PLN reasoning (50 iterations)
  - Memory usage (1000 atoms)
  - Load time
  - Large knowledge base stress test (5000 atoms)
  - Org-mode parsing (5 iterations)
  - JavaScript build time
  - Bundle size analysis
  - Regression detection (threshold: 5 seconds for 1000 atoms)
- **Artifacts**: Performance reports
- **Schedule**: Weekly on Sunday at midnight
- **Badge**: ![Performance Tests](https://github.com/drzo/emacs-aichat-skintwin/workflows/Performance%20Tests/badge.svg)

### Release Workflows

#### 13. **Release** (`release.yml`)
- **Trigger**: Version tags (v*.*.*), manual with version input
- **Purpose**: Automated release creation
- **Actions**:
  - Test execution
  - JavaScript asset building
  - Changelog extraction
  - Distribution package creation
  - GitHub release creation with artifacts
  - MELPA publishing instructions
- **Artifacts**: Release tarballs, individual `.el` files
- **Badge**: ![Release](https://github.com/drzo/emacs-aichat-skintwin/workflows/Release/badge.svg)

#### 14. **Basic Tests (Legacy)** (`test.yml`)
- **Trigger**: Push, pull request
- **Purpose**: Backwards compatibility testing
- **Note**: Maintained for compatibility; use `elisp-test.yml` for comprehensive testing
- **Matrix**: Emacs 27.2, 28.2, 29.1
- **Badge**: ![Test](https://github.com/drzo/emacs-aichat-skintwin/workflows/Test/badge.svg)

## Workflow Dependencies

```
ci-pipeline.yml
├── pre-checks
├── elisp-compile.yml
├── elisp-lint.yml
├── elisp-test.yml (depends on compile & lint)
├── org-mode-validation.yml
├── javascript-ci.yml
├── documentation-check.yml
├── code-quality.yml
└── security-scan.yml (parallel)
```

## Matrix Testing

### Emacs Versions
- **29.1** (latest stable) - Primary testing version
- **28.2** (previous stable)
- **27.2** (older LTS)

### Node.js Versions
- **20.x** (LTS, primary)
- **18.x** (older LTS)

### Operating Systems
- **ubuntu-latest** (primary)
- **macos-latest** (nightly builds only)

## Artifacts and Retention

| Artifact Type | Retention | Workflows |
|--------------|-----------|-----------|
| Compiled Elisp | 7 days | elisp-compile |
| Test results | 30 days | elisp-test |
| Coverage reports | 30 days | elisp-test, javascript-ci |
| Build artifacts | 7 days | javascript-ci |
| HTML exports | 7 days | org-mode-validation |
| Documentation | 30 days | documentation-check |
| Quality reports | 30 days | code-quality |
| Security reports | - | security-scan |
| Performance reports | 90 days | performance-tests |
| Nightly builds | 7 days | nightly-build |
| Nightly reports | 30 days | nightly-build |

## Workflow Schedules

| Workflow | Schedule | Purpose |
|----------|----------|---------|
| security-scan | Daily 2 AM UTC | Regular security checks |
| dependency-updates | Monday 9 AM UTC | Weekly dependency updates |
| nightly-build | Daily 2 AM UTC | Continuous integration |
| performance-tests | Sunday 12 AM UTC | Weekly performance baseline |

## Status Badges

Add these to your README.org or README.md:

```markdown
## Build Status

![CI Pipeline](https://github.com/drzo/emacs-aichat-skintwin/workflows/CI%20Pipeline/badge.svg)
![Elisp Lint](https://github.com/drzo/emacs-aichat-skintwin/workflows/Elisp%20Lint/badge.svg)
![Elisp Compile](https://github.com/drzo/emacs-aichat-skintwin/workflows/Elisp%20Compile/badge.svg)
![Elisp Test](https://github.com/drzo/emacs-aichat-skintwin/workflows/Elisp%20Test/badge.svg)
![Security Scan](https://github.com/drzo/emacs-aichat-skintwin/workflows/Security%20Scan/badge.svg)
```

## Triggering Workflows Manually

You can manually trigger workflows that have `workflow_dispatch`:

1. Go to the Actions tab
2. Select the workflow
3. Click "Run workflow"
4. Select branch and provide inputs (if required)

Manual trigger available for:
- CI Pipeline
- Dependency Updates
- Nightly Build
- Performance Tests
- Release

## Troubleshooting

### Common Issues

1. **Cask installation fails**: Check Emacs version compatibility
2. **NPM install fails**: Clear cache and retry
3. **Tests timeout**: Increase `initial_wait` parameter
4. **Byte-compilation warnings**: Review code for deprecated functions

### Debugging Failed Workflows

1. Check the workflow logs in the Actions tab
2. Look for the specific step that failed
3. Review error messages and stack traces
4. Run the same commands locally to reproduce
5. Check for version incompatibilities

## Contributing

When adding new workflows:

1. Follow naming conventions (`kebab-case.yml`)
2. Add proper documentation in this file
3. Include appropriate triggers
4. Set reasonable retention periods for artifacts
5. Use continue-on-error for non-critical checks
6. Add status badge references
7. Test locally before committing

## References

- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [Emacs CI Best Practices](https://github.com/purcell/setup-emacs)
- [Cask Documentation](https://cask.readthedocs.io/)
