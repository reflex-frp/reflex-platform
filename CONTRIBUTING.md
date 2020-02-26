# Contribution Guide

Contributions and issue reports are encouraged and appreciated!

- [Opening Issues](#opening-issues)
- [Submitting Changes](#submitting-changes)
  - [Guidelines for Commit Messages](#guidelines-for-commit-messages)
  - [Guidelines for Pull Requests](#guidelines-for-pull-requests)
  - [Code Quality](#code-quality)
  - [Documentation](#documentation)

## Opening Issues

Before opening an issue, please check whether your issue has already been reported. Assuming it has not:

* Describe the issue you're encountering or the suggestion you're making
* Include any relevant steps to reproduce or code samples you can. It's always easier for us to debug if we have something that demonstrates the error.
* Let us know what version of this project you were using. If you're using a github checkout, provide the git hash.
* Describe your configuration and build steps.

## Submitting Changes

### Guidelines for Commit Messages

#### Summary Line
The summary line of your commit message should summarize the changes being made. Commit messages should be written in the imperative mood and should describe what happens when the commit is applied.

One way to think about it is that your commit message should be able to complete the sentence:
"When applied, this commit will..."

##### Note on bumping dependencies

Commits that update a dependency should include some information about why the dependency was updated in the commit message.

#### Body
For breaking changes, new features, refactors, or other major changes, the body of the commit message should describe the motivation behind the change in greater detail and may include references to the issue tracker. The body shouldn't repeat code/comments from the diff.

### Guidelines for Pull Requests

Wherever possible, pull requests should add a single feature or fix a single bug. Pull requests should not bundle several unrelated changes.

### Code Quality

#### Warnings

Your pull request should add no new warnings to the project. It should also generally not disable any warnings.

#### Build and Test

Make sure the project builds and that the tests pass! This will generally also be checked by CI before merge, but trying it yourself first means you'll catch problems earlier and your contribution can be merged that much sooner!

To test your changes:
1. Fork, then clone the repo:
    ```bash
    git clone git@github.com:your-username/reflex.git
    ```

2. Make sure the tests pass:
    ```bash
    ./scripts/test
    ```

3. Make your change. Don't forget to add tests and documentation for your change! Stage or commit your changes locally, and make sure the tests pass:
    ```bash
    ./scripts/test-staged
    ```

### Documentation

#### In the code
We're always striving to improve documentation. Please include documentation for any added code, and update the documentation for any code you modify.

#### In the [Changelog](ChangeLog.md)
Add an entry to the changelog when your PR:
* Adds a feature
* Deprecates something
* Includes a breaking change
* Makes any other change that will impact users

#### In the [Readme](README.md)
The readme is the first place a lot of people look for information about the repository. Update any parts of the readme that are affected by your PR.

## Release Process
The following is the normal process by which `reflex-platform` releases are created.

- New features are merged to the `develop` branch as described above.
- A new release candidate branch is created at some point from develop, named `release/N`: with the version number for *N*.
  - Update `ChangeLog.md` with a new version number.
  - Regression test the release candidate branch.
- After QA passes:
  - Update changelog with version/date.
  - Merge `develop` to `master`.
  - Tag `master` with version.
  - Merge `master` back to `develop`.
