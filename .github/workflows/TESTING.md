# GitHub Actions Workflow Testing Guide

This guide explains how to test and verify the GitHub Actions workflows for the emacs-aichat-skintwin project.

## Local Testing

While GitHub Actions workflows are designed to run on GitHub's infrastructure, you can test some components locally.

### Prerequisites

Install the following tools:

```bash
# Emacs (version 27.2 or higher)
# Installation varies by OS

# Cask (Emacs package manager)
curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

# Node.js and npm (version 18+ or 20+)
# Use nvm or download from nodejs.org

# YAML linter (optional)
pip install yamllint
```

### Testing Workflow Syntax

Validate YAML syntax of all workflows:

```bash
# Using Python
for file in .github/workflows/*.yml; do
  python3 -c "import yaml; yaml.safe_load(open('$file'))" && echo "✓ $file" || echo "✗ $file"
done

# Using yamllint (if installed)
yamllint .github/workflows/
```

### Testing Elisp Components

```bash
# Install dependencies
cask install

# Byte-compile
cask emacs --batch -f batch-byte-compile *.el

# Run tests
cask exec ert-runner

# Load modules
cask emacs --batch -L . \
  --eval "(require 'aichat)" \
  --eval "(require 'skintwin)"
```

### Testing JavaScript Components

```bash
# Install dependencies
npm ci

# Build
npm run build

# Run tests
npm test

# Check build output
ls -lah dist/
```

### Testing Org-mode Files

```bash
# Validate Org syntax
cask emacs --batch \
  --eval "(require 'org)" \
  --eval "(find-file \"README.org\")" \
  --eval "(org-mode)" \
  --eval "(message \"Org file validated\")"

# Export to HTML
cask emacs --batch \
  --eval "(require 'org)" \
  --eval "(require 'ox-html)" \
  --eval "(find-file \"README.org\")" \
  --eval "(org-html-export-to-html)"
```

## Workflow Testing on GitHub

### Manual Workflow Triggers

Some workflows can be triggered manually:

1. Go to repository Actions tab
2. Select the workflow (e.g., "CI Pipeline")
3. Click "Run workflow" button
4. Select branch
5. Provide inputs if required
6. Click "Run workflow"

Workflows with manual triggers:
- CI Pipeline
- Dependency Updates
- Nightly Build
- Performance Tests
- Release

### Testing via Pull Requests

The safest way to test workflows is through pull requests:

1. Create a feature branch
2. Make changes
3. Push to GitHub
4. Open a pull request
5. Workflows will run automatically
6. Review results in PR checks

### Testing Specific Workflows

#### Test Elisp Lint

```bash
# Make a change to any .el file
echo "# Test change" >> aichat.el
git add aichat.el
git commit -m "test: trigger elisp-lint workflow"
git push origin your-branch
```

#### Test Org-mode Validation

```bash
# Make a change to any .org file
echo "* Test heading" >> README.org
git add README.org
git commit -m "test: trigger org-mode-validation workflow"
git push origin your-branch
```

#### Test JavaScript CI

```bash
# Make a change to any JS/JSX file
echo "// Test" >> src/main.jsx
git add src/main.jsx
git commit -m "test: trigger javascript-ci workflow"
git push origin your-branch
```

## Monitoring Workflow Runs

### Via GitHub UI

1. Navigate to repository
2. Click "Actions" tab
3. View workflow runs
4. Click on a run to see details
5. Expand job steps to see logs

### Via GitHub CLI

```bash
# Install GitHub CLI
# See: https://cli.github.com/

# List workflow runs
gh run list

# View specific run
gh run view <run-id>

# Watch a run in progress
gh run watch

# View logs
gh run view <run-id> --log
```

### Via API

```bash
# Get workflow runs
curl -H "Accept: application/vnd.github.v3+json" \
  https://api.github.com/repos/drzo/emacs-aichat-skintwin/actions/runs

# Get specific run
curl -H "Accept: application/vnd.github.v3+json" \
  https://api.github.com/repos/drzo/emacs-aichat-skintwin/actions/runs/<run-id>
```

## Debugging Failed Workflows

### Common Issues and Solutions

#### 1. Cask Installation Failure

**Symptom**: Workflow fails during `cask install`

**Solutions**:
- Check Emacs version compatibility
- Verify Cask file syntax
- Check for missing dependencies in Cask file

#### 2. NPM Installation Failure

**Symptom**: `npm ci` or `npm install` fails

**Solutions**:
- Delete `package-lock.json` and regenerate
- Check Node.js version compatibility
- Verify package.json syntax

#### 3. Test Failures

**Symptom**: Tests fail in workflow but pass locally

**Solutions**:
- Check for environment-specific code
- Verify test isolation
- Check for timing issues
- Review test logs carefully

#### 4. Build Failures

**Symptom**: Build step fails

**Solutions**:
- Verify all source files are committed
- Check for missing dependencies
- Review build logs for errors
- Test build locally

#### 5. Timeout Issues

**Symptom**: Workflow times out

**Solutions**:
- Increase timeout in workflow
- Optimize slow operations
- Check for infinite loops
- Review hanging processes

### Debugging Steps

1. **Review the logs**
   - Click on failed job
   - Expand failed step
   - Read error messages

2. **Reproduce locally**
   ```bash
   # Run the exact command from the workflow
   cask emacs --batch -L . \
     --eval "(require 'aichat)"
   ```

3. **Check workflow file**
   - Verify syntax
   - Check job dependencies
   - Verify matrix configuration

4. **Test in isolation**
   - Comment out other steps
   - Focus on failing step
   - Add debug output

5. **Compare with working runs**
   - Check previous successful runs
   - Identify what changed
   - Review commit history

### Adding Debug Output

Add debug steps to workflows:

```yaml
- name: Debug info
  run: |
    echo "Working directory: $(pwd)"
    echo "Emacs version: $(emacs --version | head -1)"
    echo "Cask version: $(cask --version)"
    echo "Node version: $(node --version)"
    echo "Files present:"
    ls -la
```

## Performance Testing

### Measuring Workflow Duration

1. Go to Actions tab
2. Select completed workflow run
3. Note total duration
4. Review individual job times
5. Identify bottlenecks

### Optimizing Workflows

**Cache dependencies**:
```yaml
- uses: actions/cache@v4
  with:
    path: |
      ~/.cask
      node_modules
    key: ${{ runner.os }}-deps-${{ hashFiles('**/Cask', '**/package-lock.json') }}
```

**Parallelize jobs**:
```yaml
jobs:
  job1:
    runs-on: ubuntu-latest
    # ...
  
  job2:
    runs-on: ubuntu-latest
    # Runs in parallel with job1
    # ...
```

**Use matrix strategically**:
```yaml
strategy:
  matrix:
    emacs-version: [27.2, 28.2, 29.1]
  fail-fast: false  # Continue even if one fails
```

## Best Practices

### 1. Start Simple
- Test with one Emacs version first
- Add matrix testing after confirming it works

### 2. Use continue-on-error
```yaml
- name: Optional check
  run: some-command
  continue-on-error: true
```

### 3. Set Reasonable Timeouts
```yaml
jobs:
  test:
    timeout-minutes: 30
```

### 4. Use Conditional Execution
```yaml
- name: Deploy
  if: github.ref == 'refs/heads/main'
  run: deploy-command
```

### 5. Secure Secrets
- Use GitHub Secrets for sensitive data
- Never commit credentials
- Use environment variables

### 6. Document Changes
- Update workflow README when changing workflows
- Add comments in workflow files
- Document required secrets

## Workflow Maintenance

### Regular Tasks

**Weekly**:
- Review failed runs
- Check for outdated actions
- Monitor performance

**Monthly**:
- Update action versions
- Review and optimize workflows
- Check dependency updates

**Quarterly**:
- Audit security settings
- Review retention policies
- Evaluate new GitHub Actions features

### Updating Workflows

1. Create a feature branch
2. Modify workflow file
3. Test changes via PR
4. Review results
5. Merge when successful
6. Monitor production runs

### Versioning Best Practices

- Use semantic versioning for releases
- Tag workflow changes in commits
- Document breaking changes
- Keep changelog updated

## Resources

### Official Documentation
- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [Workflow syntax](https://docs.github.com/en/actions/reference/workflow-syntax-for-github-actions)
- [GitHub Actions Marketplace](https://github.com/marketplace?type=actions)

### Community Resources
- [Awesome Actions](https://github.com/sdras/awesome-actions)
- [GitHub Community Forum](https://github.community/)

### Project-Specific
- [Emacs CI Examples](https://github.com/purcell/setup-emacs)
- [Cask Documentation](https://cask.readthedocs.io/)
- [Vite Documentation](https://vitejs.dev/)

## Getting Help

If workflows are failing and you can't resolve the issue:

1. Check this testing guide
2. Review workflow documentation
3. Search GitHub Issues
4. Check workflow logs carefully
5. Create an issue with:
   - Workflow name
   - Error message
   - Steps to reproduce
   - Relevant logs

## Appendix: Workflow Reference

### All Workflows

| Workflow | File | Trigger | Purpose |
|----------|------|---------|---------|
| CI Pipeline | ci-pipeline.yml | push, PR | Orchestrates all checks |
| Elisp Lint | elisp-lint.yml | push, PR | Code quality |
| Elisp Compile | elisp-compile.yml | push, PR | Byte-compilation |
| Elisp Test | elisp-test.yml | push, PR | Unit tests |
| Org Validation | org-mode-validation.yml | push, PR | Org file checks |
| JavaScript CI | javascript-ci.yml | push, PR | JS build/test |
| Security Scan | security-scan.yml | push, PR, schedule | Security checks |
| Documentation | documentation-check.yml | push, PR | Doc validation |
| Code Quality | code-quality.yml | push, PR | Quality metrics |
| Dependencies | dependency-updates.yml | schedule | Dep updates |
| Nightly Build | nightly-build.yml | schedule | Nightly tests |
| Performance | performance-tests.yml | push, PR, schedule | Benchmarks |
| Release | release.yml | tag, manual | Release creation |
| Basic Test | test.yml | push, PR | Legacy tests |

### Workflow Schedules

| Workflow | Schedule | UTC Time |
|----------|----------|----------|
| Security Scan | Daily | 2:00 AM |
| Dependency Updates | Weekly (Mon) | 9:00 AM |
| Nightly Build | Daily | 2:00 AM |
| Performance Tests | Weekly (Sun) | 12:00 AM |
