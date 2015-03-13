## Configure MHEG5 EPG Data Reader ##
Note: you only need to configure the MHEG5 EPG Data Reader once.

**DCDVDSource**
  * Extract DCDVBSource\_v0.16\_Binary.zip to a directory on your hard drive (do not delete this directory)
  * In the Filter directory, run register.bat to register the DCDVBSource filter
  * In the Plugin directory, run register.bat to register the plug-ins
  * In the Filter directory, run ChannelScan.exe Select the appropriate tuner. **It is important the correct video and audio Decoder settings are chosen. See known issue below.** Instructions how to test the .dvb will be posted soon.
  * Scan for channels using the "Channel Scan" tab. Once complete save the .dvb file.( Note: if no channels are found, shut down any programs that may be using your tuner and re-scan for channels )

## Generate XMLTV guide from MHEG5 EPG DATA using MHEG2XMLTV ##
**MHEG2XMLTV**
  * Extract mheg2xmltv-1.x.x.x-binary.zip and open a command prompt e.g. Windows XP --> Start --> Run --> cmd
  * Change the directory of the command prompt to point to the directory where "mheg2xmltv.exe" can be found
    * At the command prompt enter for example: mheg2xmltv.exe c:\xmlDump\tvguide.xml c:\scannedTuners\tunerOne.dvb c:\scannedTuners\tunerTwo.dvb
      * mheg2xmltv.exe takes an output parameter and multiple input parameters
        * output = the path to the generated xml file
        * input(s) = the path to the scanned tuner .dvb files. When specifying multiple tuners(inputs), if the first tuner fails to grab the EPG data because it is busy for more than 5 minutes the next tuner will be used.
**Known issue** "An exception occurred: Unspecified error" If you encounter this error please create a new .dvb file using a different audio or video decoder setting with DCDVDSource. Run mheg2xmltv.exe with the new .dvb file. We will try resolving this issue as soon as possible. Thanks.

**TIP**
Create a scheduled task to run MHEG2XMLTV weekly.

## Command Syntax ##

**`mheg2xml output input`**

### Windows: ###
input = path to a DC-DVB Source .dvb file or a path to a DSM-CC dump

### Linux: ###
input = path to a channels.conf file or a path to a DSM-CC dump

### Other: ###
input = a path to a DSM-CC dump

### All: ###
output = path to save XML-TV file