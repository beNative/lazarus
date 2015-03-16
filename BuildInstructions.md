# Introduction #

This page explains how you can build your own copy of the Notepas binary from the svn sources with an automated buildscript which is based on the great [fpcup](https://bitbucket.org/reiniero/fpcup) project.
The idea is to invoke a single executable that will automatically:
  * download the FPC and Lazarus sources for your platform from the source Subversion/SVN repositories,
  * download and install all 3rd party packages needed to build Notepas.
  * make a fresh Lazarus build
  * make a fresh Notepas build for your platform