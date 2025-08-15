#!/bin/bash
set -euo pipefail

# if [[ -f ~/.minecraft/versions/.jar ]]; then
#     infdev=~/.minecraft/.jar
# el
if [[ -e "${2}" ]]; then
    infdev="${2}"
elif [[ -f ~/.betacraft/versions/a1.1.2_01.jar ]]; then
    infdev=~/.betacraft/versions/a1.1.2_01.jar
else
    printf "unable to locate a .jar with a working infdev level previewer"
    exit 1
fi

class_path="${infdev}"

working_dir="$(realpath "${1}")"

jshscript='import java.applet.Applet;
import javax.swing.JFrame;
import java.io.File;
import java.lang.reflect.Field;
int pre_invoke_count = Thread.activeCount();
Class<?> isomPreviewAppletClass = Class.forName("net.minecraft.isom.IsomPreviewApplet");
Field appletField = isomPreviewAppletClass.getDeclaredFields()[0];
appletField.setAccessible(true);
Applet applet = (Applet) isomPreviewAppletClass.newInstance();
Field workingDirField;
for (Field field : appletField.getType().getDeclaredFields()) {
    if (field.getType() == File.class) {
           workingDirField = field;
           break;
       }
}
workingDirField.setAccessible(true);
workingDirField.set(appletField.get(applet), new File("'"${working_dir}"'"));
JFrame frame = new JFrame("IsomPreview");
frame.add(applet);
frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
frame.setSize(854, 480);
frame.setLocationRelativeTo(null);
frame.setVisible(true);
applet.start();
while (Thread.activeCount() > pre_invoke_count) {
      Thread.sleep(200);
}'

jshell --class-path "$(dirname "${0}")"'/resources/:'"${class_path}" - <<<"${jshscript}"
