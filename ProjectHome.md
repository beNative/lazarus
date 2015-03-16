# Downloads #

## The primary download location has been moved to [SourceForge](https://sourceforge.net/projects/notepas/) ##

The sources are still based here but that might change too in the near future as I'm looking at a more advanced version control system like [Git which supports advanced workflows to streamline release management](http://nvie.com/posts/a-successful-git-branching-model/).

# Introduction #

**Notepas** is a fast portable native multiplatform text editor written in [Lazarus](http://www.lazarus.freepascal.org/) and can be compiled for multiple platforms and widget sets using the advanced native [Free Pascal Compiler](http://www.freepascal.org/). Aimed towards developers it is equipped with some functions usually not found in other text editors and introduces a couple of new **exclusive features:**
  * **Alpha blended coloring** supporting multiple layers of transparency for both text and background.
  * **Code filter**: a tool to filter lines that match a given search string or regular expression.
  * **Code shaper**: a set of tools to apply advanced formatting rules to a text selection.
  * **Smart select**: make (customizable) selections that depend on the active highlighter and context.
  * **Align lines**: align multiple lines based on tokens found in the current selection.
  * **Common Syntax coloring rules** for all registered highlighters.
  * **Advanced docking** support.

**Other features:**
  * **Single native** and **fast** executable, no external libraries required.
  * **Portable**, as settings are stored in a single XML-file.
  * **Syntax highlighting**.
  * **Code folding** depending on syntax (currently for XML, HTML and Pascal code but will be extended to support all highlighters).
  * **Multi-line comment folding**.
  * **Selection folding.**
  * **Region folding.**
  * **Synchronized edit**.
  * **Line modification indicators**.
  * Line numbers display.
  * **Smart searching** of the word at the caret position (`CTRL-ALT-UP/CTRL-ALT-DOWN`)
  * Highlighting of words matching the selected word.
  * Highlighting of text matching the selected block.
  * Highlighting of matching bracket.
  * Routine seperators.
  * Highlighting of search matches.
  * Comment and uncomment (for both line and block comments) handling depending on active highlighter.
  * Column selection mode.
  * Supports **user definable highlighters**. Highlighter rules can be specified using XML.
  * Extensive set of keyboard shortcuts allowing fast navigation and intuitive text editing.
  * Drag and drop support for both files and code selections.
  * Search & replace in multiple opened files with regular expression support.
  * Support for multiple encoding schemes (Unicode and many others).
  * Support for multiple line endings (Windows, Unix, Mac).
  * Zoom.
  * Localization support using simple text files that are loaded by the application at runtime (i18n using [GetText](https://www.gnu.org/software/gettext/)).
  * Monitor external changes.
  * (Experimental) Bookmark support.
  * (Experimental) support for both internal and external code formatters.
  * (Experimental) support for Pascal scripting.
  * (Experimental) Hex editor.
  * (Experimental) HTML viewer.
  * (Experimental) structure viewer.
  * (Experimental) support for multiple views on the same source buffer.
  * (Experimental) support for keyboard macros.
A lot of other unique features will be added soon so stay tuned as the code evolves.
For now only preview builds for the Windows, Linux and MacOS X (Darwin) platforms are available, but native FreeBSD and Solaris builds are in the pipeline.

Notepas is a work-in-progress. If you have bug reports or suggestions for new features please contact me. I am not able to test every feature on every supported platform so there might be many annoyances that will appear for your specific needs that I'm not aware of. Let me know and I'll tackel it ASAP.
Experimental features are activated in debug mode and provide a preliminary look to what's currently cooking in the kitchen.

# Screenshots #

## Code filter ##

![http://notepas.googlecode.com/svn/wiki/images/notepas%20-%20codefilter.png](http://notepas.googlecode.com/svn/wiki/images/notepas%20-%20codefilter.png)

## Docking multiple views ##

![http://notepas.googlecode.com/svn/wiki/images/notepas%20-%20docking.png](http://notepas.googlecode.com/svn/wiki/images/notepas%20-%20docking.png)

## Search in multiple views ##

![http://notepas.googlecode.com/svn/wiki/images/notepas%20-%20search.png](http://notepas.googlecode.com/svn/wiki/images/notepas%20-%20search.png)

## Align lines ##

![http://notepas.googlecode.com/svn/wiki/images/notepas%20-%20align%20lines.png](http://notepas.googlecode.com/svn/wiki/images/notepas%20-%20align%20lines.png)

## Code shaper ##

![http://notepas.googlecode.com/svn/wiki/images/notepas%20-%20codeshaper.png](http://notepas.googlecode.com/svn/wiki/images/notepas%20-%20codeshaper.png)

## Character Map ##

![http://notepas.googlecode.com/svn/wiki/images/notepas%20-%20character%20map.png](http://notepas.googlecode.com/svn/wiki/images/notepas%20-%20character%20map.png)

## Synchronized edit ##

![http://notepas.googlecode.com/svn/wiki/images/notepas%20-%20synchronized%20edit.png](http://notepas.googlecode.com/svn/wiki/images/notepas%20-%20synchronized%20edit.png)

## Code folding ##

![http://notepas.googlecode.com/svn/wiki/images/notepas%20-%20code%20folding.png](http://notepas.googlecode.com/svn/wiki/images/notepas%20-%20code%20folding.png)

## Line modification indicators ##

![http://notepas.googlecode.com/svn/wiki/images/notepas%20-%20changes%20gutter.png](http://notepas.googlecode.com/svn/wiki/images/notepas%20-%20changes%20gutter.png)

## Editor pop-up menu ##

![http://notepas.googlecode.com/svn/wiki/images/notepas%20-%20popup%20menu.png](http://notepas.googlecode.com/svn/wiki/images/notepas%20-%20popup%20menu.png)

**A preview is available for download. Development is now focusing on the first stable release version.**

