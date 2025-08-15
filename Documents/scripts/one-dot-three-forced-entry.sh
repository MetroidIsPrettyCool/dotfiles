#!/bin/bash
set -euo pipefail

if [[ -f ~/.minecraft/versions/1.3/1.3.jar ]]; then
    one_dot_three=~/.minecraft/versions/1.3/1.3.jar
elif [[ -f ~/.betacraft/versions/1.3-pre-1249.jar ]]; then
    one_dot_three=~/.betacraft/versions/1.3-pre-1249.jar
else
    printf "unable to locate a 1.3 pre-release .jar"
    exit 1
fi

# class_path=~/.betacraft/bin/lwjgl.jar:~/.betacraft/bin/lwjgl_util.jar:~/.betacraft/bin/jinput.jar:"${one_dot_three}"
class_path="${one_dot_three}"

jshscript='import java.lang.reflect.*;
import java.lang.Thread;
int pre_invoke_count = Thread.activeCount();
Class.forName("'"${1}"'").getDeclaredMethod("a", String[].class).invoke(null, (Object)new String[] {});
while (Thread.activeCount() > pre_invoke_count) {
      Thread.sleep(200);
}'

jshell --class-path "${class_path}" - <<<"${jshscript}"
