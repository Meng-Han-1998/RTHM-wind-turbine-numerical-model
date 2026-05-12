@ECHO off
SET IncludeFile=..\gitVersionInfo.h

<NUL SET /p IncludeTxt=#define GIT_VERSION_INFO '> %IncludeFile%
FOR /f %%a IN ('git describe --abbrev^=8 --always --tags --dirty') DO <NUL SET /p IncludeTxt=%%a>> %IncludeFile%
git describe --abbrev^=8 --always --tags --dirty > NUL
IF %ERRORLEVEL%==0 ( ECHO '>> %IncludeFile% ) else ( ECHO Unversioned from 6b8706b75698cb1c97c6a8e3e9f42538a63fa71b '>> %IncludeFile% )

EXIT /B 0