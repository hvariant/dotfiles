import sys
import os
import subprocess

output = subprocess.check_output("xinput --list",shell=True)
output = output.decode("utf-8")

touchpad_id = -1
for line in output.split("\n"):
    #if line.find("DLL075B:01") >= 0:
    if line.find("Synaptics") >= 0:
        words = line.split()
        for word in words:
            if word.startswith("id="):
                touchpad_id = int(word.split("=")[1])
                break

        break

if touchpad_id == -1:
    print("failed to get touchpad device id")
    sys.exit(1)

output = subprocess.check_output("xinput --list-props %d" % touchpad_id,shell=True)
output = output.decode("utf-8")

touchpad_state = -1
for line in output.split("\n"):
    if line.find("Device Enabled") >= 0:
        touchpad_state = int(line.split()[-1])
        print(touchpad_state)
        break

if touchpad_state == 0:
    os.system("xinput enable %d" % touchpad_id)
else:
    os.system("xinput disable %d" % touchpad_id)
