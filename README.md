# Better Translation Manager

https://github.com/andersmelander/better-translation-manager  
Download here: https://github.com/andersmelander/better-translation-manager/releases

The Better Translation Manager (BTM) is a replacement for the Delphi Translation Manager a.k.a. the Integrated Translation Environment (ITE) and External Translation Manager (ETM).

![BTM in action](Artwork/amTranslationManager-main.png)

## Why?

The standard Translation Manager that ships with Delphi today was originally an individual product known as the Borland Translation Suite. With [Delphi 5 it became a part of the enterprise edition](https://web.archive.org/web/20020220081654/http://www.borland.com:80/delphi/del5/feaben/translation.html).  
The Borland Translation Suite showed great promise but unfortunately it never evolved from its roots as an external tool and has always been hampered by severe bugs that made it completely unusable in practice. As a result nobody uses it as can be witnessed by the plethora of homegrown and commercial alternatives.

The great benefit of the standard translation system is that it just works (this is the system itself I'm talking about, not the tools. The tools suck). Apart from the requirement that you must use  resourcestrings you don't need to do anything special when writing your code. At run time you just place the compiled resource modules in the same folder as your application and the Delphi Run Time Library automatically takes care of loading and using the translations based on the current Windows user interface language.

Anyway, since Embarcadero has now finally admitted that they are never going to fix the Delphi Translation Manager and instead recommend that we find alternative solutions, I decided that it was time I solved this little problem once and for all.

The core functionality of the  Better Translation Manager was written in two weeks during my summer vacation in Italy 2019. Amazing what you can do with a little pasta!


## Features

* Does not require any changes to the source code of the application being translated.
* Works with the existing standard Delphi localization system.
* Translates resourcestrings and all strings in forms regardless of any 3rd party components used.
* Works on compiled application. Source code is never used.
* Generates localized binary resource modules (resource DLLs). Does not use an external compiler.
* Can import existing translations from compiled application and resource modules or from XLIFF localization source files (dfn, rcn files).
* Read and save TMX and TBX translation memory files.
* Import Translation Memory from TMX (Translation Memory eXchange), TBX (TermBase eXchange), Microsoft Glossary and CSV.
* Machine Translation using Translation Memory, Microsoft Translation Service or DeepL.
* Forms, Components, Types and Values that should be ignored can be specified in a Stop List.
* Translations are Spell Checked.
* Validation Rules to catch common translation mistakes.
* Supports Right To Left (RTL) editing based on translation language.
* Translation project is stored in a single XML file.
* Command line interface for use in automated build systems.
* Fast! Refreshing a large project typically takes less than a second vs. many minutes with the ITE/ETM.
* Supports all Unicode versions of Delphi (i.e. Delphi 9 and later).
* Resource modules contain the version resource of the source  application.


## What it doesn't do

There's one task that BTM, by design, doesn't attempt to solve: Localizing the placement and size of controls.

Since it has been my experience that it is a far better idea to design the user interface in such a way that the layout automatically accommodates changes in font- and text size and shorter/longer texts due to translation, I decided from the start that I would not be supporting localization of size and position of controls. This also relieved me of having to create a run time form designer, supporting 3rd party controls visually (something that nobody so far has managed to find a foolproof solution to) and deciding what individual properties constitutes size/position values.

Instead I just localize all string values - and only string values.


## Typical Workflow

1. Compile target application.
2. Open translation project in BTM.
3. Update translation project.
4. Translate any new string values.
5. Build resource modules.

## Getting Started

1. Compile your application.
   Make sure that the **Output resource string .drc file** linker option has been set.
2. Start BTM.
3. Select **New project**,  enter the path to your compiled application and specify the current language of the application.
4. Select the desired translation language in the Target field.
5. Select the module (form or resourcestrings) to translate.
6. Translate individual text values.
7. Save project.
8. Build Resource Modules.

## Deploying a Localized Application

To deploy a localized application all you need to do is make sure that the resources modules are placed in the same directory as the application executable.

Say you have localized the HelloWorld sample application; You have compiled the application, created a translation project, translated the texts to Danish and German and built the resource modules. You should now have `HelloWorld.exe`, `HelloWorld.DAN` and `HelloWorld.DEU` in one directory.  
The native (or source) language of the application is US English, so if you run the application on a Windows where the regional settings have been configured as English (United States), then no resource module is loaded. All texts are  already as they should be.  
However if you run the application where the regional settings have been configured as "German (Germany)", then the `HelloWorld.DEU` resource module will be automatically loaded and all translated texts will appear in German. Same principle for Danish; The `HelloWorld.DAN` resource module will be loaded if regional settings have been configured as "Danish (Denmark)".  
If no resource module is found, that matches the language of the  regional settings, then the application will always just fall back to the native language.

The point is that the Delphi Run Time Library already knows how to load resource modules and how to determine which resource module, if any, should be loaded based on the regional settings of the use.  
You can read more about this in the Delphi help: http://docwiki.embarcadero.com/RADStudio/Rio/en/Deploying_Localized_Applications [^1]

### Region Neutral translations

One point that the Delphi help doesn't emphasize but which could be important for you is that you can create region neutral resource modules.

When you specify a source or target language you have to specify it as a regional language. That is you cannot just specify the language as "English". You have to include a region. E.g. "English (United States)" or "English (United Kingdom)" etc.  
Correspondingly, if you have a resource module named `HelloWorld.ENU` then that resource module will only be used for US English. If all you wanted was to provide an English translation and you don't really care about regions, dialects and whatnot then in principle you would have to provide resource module for each of the 16 regional variants of English currently supported by Window.

Luckily the developers at Borland were smart enough to anticipate this problem when they implemented the resource module loading logic (see the `GetResourceModuleName` function in the  `System` unit); When the resource module loader looks for resource module files it first looks for the region specific filenames and then for the region neutral filenames [^2].

So returning to our example above, in order to make `HelloWorld.ENU` region neutral you just remove the region part from the filename: `HelloWorld.EN` and now the resource module will be used for all variants of English.

## The Application

In case you just want to use BTM as-is, and don't care to compile it yourself, you can download the application installer here: https://github.com/andersmelander/better-translation-manager/releases

Note that the compiled application will almost always lag behind the source code since it has to go through a bit of QA before I upload it. The application has been virus checked with [VirusTotal](https://www.virustotal.com).

The installer contains the main application, the command line tool, a sample translation memory database and a few spell check dictionaries.

Settings are stored in the registry under `HKCU\Software\Melander\TranslationManager` and files are stored in the `%APPDATA%\TranslationManager` folder.

### Spell Check dictionaries

The spell checker uses dictionaries in the [HunSpell](https://en.wikipedia.org/wiki/Hunspell) format.

It is up to yourself to [find](https://www.google.com/search?client=firefox-b-d&q=hunspell%20dictionary) and download dictionaries.  
To add a new dictionary you must copy the dictionary files to the `%APPDATA%\TranslationManager\Dictionaries` folder.  
The dictionary for a language is contained in two files; `language.dic` and `language.aff` where "language" is specified as one of the [ISO639-2](https://en.wikipedia.org/wiki/ISO_639-2), [ISO639-1](https://en.wikipedia.org/wiki/ISO_639-1) or [RFC 4646](https://tools.ietf.org/html/rfc4646)/[IETF](https://en.wikipedia.org/wiki/IETF_language_tag) language codes.  
For example the dictionary files for Danish should be named `dan.dic` and `dan.aff`, `da.dic` and `da.aff` or `da-DK.dic` and `da-DK.aff` - or any combination of these.

You can find some good collections of open source dictionaries here:

* https://github.com/wooorm/dictionaries
* https://github.com/LibreOffice/dictionaries
* https://web.archive.org/web/20231215134708/https://www.softmaker.com/en/download/dictionaries  
  (the .sox files you can download here are in fact zip files. You will find the .dic and .aff files inside)

Just remember to rename the dictionary files to fit the above rules.

## The Source

[The source](https://github.com/andersmelander/better-translation-manager/tree/master/Source) is available primarily so you can build your own custom versions and to ensure that BTM can be updated if I should get hit by a bus.

I will accept [pull requests](https://github.com/andersmelander/better-translation-manager/pulls) for bug fixes provided I can reproduce the problem or it is obvious.  
Pull requests for new features or other changes should be backed by well argued use cases. Start by [creating an issue](https://github.com/andersmelander/better-translation-manager/issues).

### License

The source code is released under the MPL 2.0 license:

> Copyright © 2019-2021 Anders Melander  
> This Source Code Form is subject to the terms of the Mozilla Public  
> License, v. 2.0. If a copy of the MPL was not distributed with this  
> file, You can obtain one at http://mozilla.org/MPL/2.0/.

### Requirements

BTM has been tested with the following versions but probably works with older (and newer) versions too:

* Delphi 12.1
* DevExpress VCL version 24.1.3
* madExcept 5.1.4

### Dependencies

The following commercial 3rd party libraries are required in order to compile the source:

* [DevExpress VCL](https://www.devexpress.com/products/vcl/)
* [madExcept](http://madshi.net/) (optional)

The libraries must in the Delphi default library search path.

## Command Line Interface

For use in automated build systems the `amResourceModuleBuilder` command line utility is provided. Using this tool you can build resource module for specific languages or for all languages supported by a given translation project.

    Usage: amResourceModuleBuilder <projectfile> [options]
    
    Options:
      -?                 Display help (this message)
      -t:<language>      Only build for specified language
      -b                 Build resource module(s)
      -s:<source file>   Specify source file
      -y:<symbol file>   Specify string symbols file
      -o:<output folder> Specify resource module output folder
      -v                 Display verbose messages
      -n:<scheme>        File name scheme:
                           0: ISO 639-2 (e.g. ENU, DAN, DEU, etc)
                           1: ISO 639-1 (e.g. EN, DA, DE, etc)
                           2: RFC 4646 (e.g. en-US, da-DK, de-DE, etc)

[^1]: The Delphi help uses an example with resource module files named Test.exe.fr-FR and Test.exe.de-DE. This example is wrong. The correct file names for that example would be Test.fr-FR and Test.de-DE.
[^2]: One of the recent Delphi versions has unfortunately somewhat broken this logic when support for [Language Culture names](https://en.wikipedia.org/wiki/IETF_language_tag) was added. The current search order is now: #1 [RFC 4646](https://tools.ietf.org/html/rfc4646) with region, #2 RFC 4646 without region, #3 [ISO 639‑2](https://en.wikipedia.org/wiki/ISO_639-2) (includes region), #4 [ISO 639‑1](https://en.wikipedia.org/wiki/ISO_639-1) (no region). Since the language code of RFC 4646 is in fact ISO 639-1, search order  #1 and #3 are identical and region neutral translations now has precedence over region specific ones. [This is clearly a bug](https://quality.embarcadero.com/browse/RSP-26515).