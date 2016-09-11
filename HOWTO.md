
This document is intended to be a collection of common tasks when contributing to CSPLib.
Please feel free to suggest modifications and improvements via the issue tracker (especially if something is wrong!).


# System

## Setting up passwordless GitHub

By default GitHub works by prompting to ask your credentials for every push.
This can become annoying quite quickly.
However, setting passwordless access to GitHub (per computer) is easy!

These instructions will work for Linux and Mac OS X.

1. **Check if you have an SSH key-pair in your system.** It is a good idea to check if you have this first, since a lot of people need this for one reason or another. You can check whether you have an SSH key-pair by checking whether you have the following two files or not: `~/.ssh/id_rsa` and `~/.ssh/id_rsa.pub`. The former is your private key (don't give it to anybody!) and the latter is your public key.
2. If you don't have the key-pair, you can create it by running `TODO`. Now you should have the two files mentioned above.
3. Upload the contents of your public key file to GitHub by navigating to the TODO section under your GitHUb settings page.

That's it!


# Typesetting

## Linking to other problems

Including the problem number (together with the "prob" prefix) inside curly brackets creates a link to the given problem.
The text of the hyperlink will be the full name of the problem.

For example, writing `{prob006}` inside a specification file ("specification.md") will create a link with the hyperlink text "Golomb rulers".
