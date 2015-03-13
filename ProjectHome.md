# mheg2xmltv #
_mheg2xmltv is a [XmlTv](http://xmltv.org/) grabber for [Freeview NZ](http://freeviewnz.tv/) [MHEG-5](http://www.impala.org/) EPG data._

Runs on [.NET 2.0](http://www.microsoft.com/downloads/details.aspx?FamilyID=0856EACB-4362-4B0D-8EDD-AAB15C5E04F5) for Windows or [Mono](http://www.mono-project.com/) for Linux and Mac.

On Windows uses [DC-DVB Source](http://www.dsp-worx.de/?n=11) to grab the MHEG data, and uses [rb-download](http://redbutton.sourceforge.net/) on Linux. All platforms support reading the data from an existing dump folder.

**NEW:**
I just found out about this: [EPG Collector](http://sourceforge.net/projects/epgcollector/)
It was inspired by this program, and is much more up to date, better maintained and has a large active community of users.
It also supports other countries such as Australia and the UK.