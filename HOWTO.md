
This document is intended to be a collection of common tasks when contributing to CSPLib.
Please feel free to suggest modifications and improvements via the issue tracker (especially if something is wrong!).


# System

## I am an absolute beginner, how do I make a change?

First of all: Welcome!

We realise all these technologies (version control, Git, GitHub, typesetting with Markdown, etc) can be intimidating if you have never done anything like it before.
Yet you are here and you want to make a contribution to CSPLib.
So nice of you!

Here, we will give step by step instructions describing how you can make a contribution to CSPLib.
These instructions will use the command line [git tool](https://git-scm.com), there are graphical user interfaces which may or may not make life easier.

We strongly suggest reading tutorials on Git and GitHub usage to learn these tools better.

1. **Create a GitHub account.** Follow the instructions on the [GitHub website](http://www.github.com).
2. **Create a fork of CSPLib.** This means creating a copy of the CSPLib repository under your GitHub username, hosted on GitHub. You can create a fork by going to the main [CSPLib repository](https://github.com/csplib/csplib) page and clicking the "Fork" button (top right).
3. **Clone your fork to your computer.** Decide where you want to have the local copy of CSPLib on your computer. Navigate to this location and run `git clone git@github.com:USERNAME/csplib` (replacing `USERNAME` with your username).
4. **Make a change.** Edit an existing problem, add data files, add new results, or add a new problem! Use your favourite tools to make the modifications.
5. **Build and preview the webpage locally.** Run `make`, followed by `make serve` to build and serve your modified version of CSPLib locally. Go to http://localhost:8000 in your browser to see what everything looks like!
6. **View the changes you've made.** You can use the following command to see the current status of your working copy of CSPLib: `git status`
7. **Make a branch.** Use the following command to create a new named branch: `git checkout -b BRANCH_NAME` (where BRANCH_NAME is the branch name you want to use).
   (This step is optional, but it is good practice.)
8. **Commit your changes.** Use `git add` to add new files, and use `git commit -m COMMIT_MESSAGE` to commit your changes.
9. **Repeat,** Repeat steps 4 to 8 as many times as necessary.
10. **Push to your fork.** Use `git push origin` to push your local changes (commits) to your fork on GitHub.
11. **Create a pull request,** Go to the webpage for your fork. It will be at `https://github.com/USERNAME/csplib` (where USERNAME is your GitHub username). Switch to your branch using the dropdown menu on the page. Then click the "New pull request" button. GitHub will show you a preview of your changes and will allow you to write a message to CSPLib maintainers before creating the pull request.
12. TODO: build bot
13. TODO: you can make more commits

It is quite a lot of steps isn't it?
Thankfully these are transferrable skills, most open source software projects use a similar setup.
Once you are familiar with the workflow you can contribute to other projects too!

Happy hacking!


## Keeping your fork up to date

If you made a fork of CSPLib and make a clone of your fork to your local computer, you can keep your fork up to date by running the following commands.

    git fetch upstream
    git checkout master
    git merge upstream/master
    git push origin master

Note that you need to set `origin` to point to your fork, and `upstream` to point to the main CSPLib repository for this to work. You can set these by running the following commands.

    git remote add origin https://github.com/<USERNAME>/csplib.git
    git remote add upstream https://github.com/csplib/csplib.git


## Setting up passwordless GitHub

By default GitHub asks your credentials for every push.
This can become annoying quite quickly.
However, setting passwordless access to GitHub (for a specific computer) is easy!

These instructions will work for Linux and Mac OS X.

1. **Check if you have an SSH key-pair in your system.** It is a good idea to check if you have this first, since a lot of people need this for one reason or another. You can check whether you have an SSH key-pair by checking whether you have the following two files or not: `~/.ssh/id_rsa` and `~/.ssh/id_rsa.pub`. The former is your private key (don't give it to anybody!) and the latter is your public key.
2. If you don't have the key-pair, you can create it by running `TODO`. Now you should have the two files mentioned above.
3. Upload the contents of your public key file to GitHub by navigating to the TODO section under your GitHUb settings page.

That's it!


## Simple edits

There is a simpler way to make "simple edits" (e.g. I just want to add a bibtex reference, or improve the description). These can be done completely though the GitHub interface using the "Edit Page" link, which exists on most pages.

Just click the edit button, make your changes through the web interface and GitHub will create a pull request automatically with your changes.


# Typesetting

## Linking to other problems

Including the problem number (together with the "prob" prefix) inside curly brackets creates a link to the given problem.
The text of the hyperlink will be the full name of the problem.
For example, writing `{prob006}` inside a specification file ("specification.md") will create a link with the hyperlink text "Golomb rulers".

As an alternative, using square brackets instead of curly brackets will create a link to the problem but will use the problem number as the hyperlink text.
Modifying the previous example, writing `[prob006]` will create a link with the hyperlink text "prob006".


## Citing references

Include the bibtex entry for a reference under `references/references.bib`, and you can cite it using the following syntax: `cite{KEY}` (where `KEY` is the bibtex key).

So it is just like citing references in Latex, except the leading backslash character is missing (so not `\cite{}` but `cite{}`).

