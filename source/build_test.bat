@echo off

if not exist ..\build mkdir ..\build

cl test.generated.c ^
	/Fe..\build\ /Fd..\build\ /Fo..\build\ ^
	/nologo /Od /Zi /MTd ^
	/link /subsystem:console
