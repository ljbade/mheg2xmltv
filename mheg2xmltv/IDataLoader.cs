using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace mheg2xmltv
{
    interface IDSMCCReader
    {
        Stream GetCarouselFile(string path);

        string[] GetCarouselFiles(string path);

        string[] GetCarouselDirectories(string path);
    }
}
