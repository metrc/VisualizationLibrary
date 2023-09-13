# Github Actions Workflows

This folder contains workflow files for [Github Actions](https://docs.github.com/en/actions) (GA). Github Actions is a continuous integration/continuous delivery (CI/CD) tool that can be used to automate builds and perform testing. This `README.md` file contains additional documentation relating to the GA workflows that are used by VisualizatonLibrary, as well as development instructions on how to test GA workflows.

## VisualizatonLibrary GA workflow overview

The VisualizatonLibrary repository uses GA workflows for the following purposes:

* Running `R CMD check` on packages
* Building binary packages for Linux and Mac OS.
* Running unit tests (currently in progress).

Right now, the process of building package releases is done in `build-*-releases.yml`. A build consists of three major steps:

1. First, we run the build on linux. Linux builds are canaries. If they do not succeed, then we abort the build process altogether, and do not proceed on to MacOS and Windows builds. This helps save on execution credits.
2. If the linux build succeeds, we proceed to create a git tag, and a corresponding github release. We generate unique tags for each build, using [SemVer](https://semver.org/) versioning principles.
3. Build artifacts are uploaded to the release. This is done for every platform.

There are two separate workflow files, one for development releases on branch `dev`, and one for production releases on branch `main`. They are mostly identical.

## Local Development

It can be a little tricky iterating and testing GA workflows, because in order to test a workflow, a commit has to be pushed to the repository and then run. This can lead to broken GA commits being pushed, as the developer iterates upon the workflow. A much better way to approach GA development, is to run the workflows locally using a local task runner.

### Using nektos/act to run GA workflows locally

The best way to do this is to use [`act`](https://github.com/nektos/act), a tool to run GA workflows on your local machine with Docker. In order to develop GA workflows locally, first install Docker Engine, and then act.

* [Docker Engine](https://docs.docker.com/desktop/install/linux-install/)
* [nektos/act](https://github.com/nektos/act)

Please note that when installing act using the bash installation method (i.e. `install.sh`), the resulting act binary will be placed within a `bin/` directory in your current directory. In order for the act binary to be available on your `$PATH`, you will need to either add the binary's path manually to your `$PATH` *or* move it to a location that is globally available, like `/usr/bin/`.

Once act is installed, it must be run in the repository directory (e.g. `VisualizatonLibrary/`). Act will aim to autodiscover the `.github/workflows/` directory on it's own. 

In order to test the workflows within the VisualizatonLibrary repository, you must execute act with a [Github personal access token](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token) (e.g. `GITHUB_TOKEN`). This is because our workflows include actions which automatically interface with Github, such as creating a new release.

```bash
cd VisualizatonLibrary
act -s GITHUB_TOKEN
```

The above command will request a Github personal auth token interactively, before proceeding to run all workflows.

### Using actionlint to statically analyse GA workflows

Another development tool that will be useful for working with GA workflows is [actionlint](https://github.com/rhysd/actionlint), a static analyser for Github Actions. Actionlint is available both via an [online checker](https://rhysd.github.io/actionlint/), as well as a separate [VSCode plugin](https://github.com/arahatashun/vscode-actionlint).

By using actionlint and act, the development velocity for GA workflows can be greatly increased.

## Self-Hosted Runner

Further information to be added.

* https://docs.github.com/en/actions/hosting-your-own-runners/about-self-hosted-runners
* https://docs.github.com/en/actions/hosting-your-own-runners/adding-self-hosted-runners
* https://docs.github.com/en/actions/hosting-your-own-runners/using-self-hosted-runners-in-a-workflow
