import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.PrintStream;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.TimeZone;

/**
 * @author Milenko Mitrovic <dcoder@dsp-worx.de>
 * 
 * This example shows the internal Format of the EPG (EPG.data)
 * which is used by the DC-DVB Source
 */
public class EPGParser 
{
	static int 			EPG_HEADER 		= 0x00475045;
	static int 			EPG_VERSION		= 0x00000001;
		
	public static int readInt(InputStream reader) throws Exception
	{
		byte b[] = new byte[4];
		reader.read(b);
		return (b[0]&0xff)|((b[1]&0xff)<<8)|((b[2]&0xff)<<16)|((b[3]&0xff)<<24);
	}
	
	public static byte readByte(InputStream reader) throws Exception
	{
		byte b[] = new byte[1];
		reader.read(b);
		return b[0];
	}
	
	public static boolean readBoolean(InputStream reader) throws Exception
	{
		byte b[] = new byte[1];
		reader.read(b);
		return b[0] == 0x01;
	}

	public static String readString(InputStream reader) throws Exception
	{
		int size = readInt(reader);
		if (size > 0)
		{
			byte b[] = new byte[size];
			reader.read(b);
			return new String(b);
		}
		
		return "";
	}

	public static String getDateTime(double time)
	{
		Calendar c = Calendar.getInstance();
		// convert from UTC Time to Local Time
		c.setTimeZone(TimeZone.getTimeZone("GMT"));
		// convert from Delphi Time
		c.setTimeInMillis((long)((time - 25569) * 86400000));

        SimpleDateFormat sdf = new SimpleDateFormat();
        return sdf.format(c.getTime()); 			
	}
	
	public static double readDateTime(InputStream reader) throws Exception
	{
		byte b[] = new byte[8];
		reader.read(b);
		return ByteBuffer.wrap(b).order(ByteOrder.LITTLE_ENDIAN).getDouble();	
	}

	public static void main(String[] args) 
	{
		PrintStream out = System.out;

		System.out.println("DC-DVB EPG Reader for EPG Versions <= " + EPG_VERSION);
		System.out.println(" ");
		
		if (args.length < 1)
		{
			System.err.println("You need to specify the Path to the EPG.data File");
			return;
		}
		
		// Open the File and check if it exists
		File file = new File(args[0]);
		if (!file.exists())
		{
			System.err.println("Filename doesn't exist");
			return;
		}
		
		// A valid EPG File is at least 16 bytes large
		if (file.length() < 16)
		{
			System.err.println("A valid EPG File is at least 16 bytes large");
			return;
		}

		// create the Reader Object to read from the File
		try 
		{
			FileInputStream reader = new FileInputStream(file);
			
			// check EPG Header
			if (readInt(reader) != EPG_HEADER)
			{
				System.err.println("EPG Header is wrong !");
				return;				
			}
			
			// check EPG Version
			int version = readInt(reader);
			if (version > EPG_VERSION)
			{
				System.err.println("EPG Version is not supported !");
				return;				
			}
			
			// next in the Header is the number of Services, followed by the Filesize
			int serviceCount = readInt(reader);
			int fileSize = readInt(reader);
			
			if (file.length() != fileSize)
			{
				System.err.println("Corrupted EPG File !");
				return;				
			}

			out.println("Number of Services: " + serviceCount);
			out.println(" ");

			for (int i = 0; i < serviceCount; i++) 
			{
				out.println(" Service " + (i+1));
				out.println(" ");
				
				int sid = readInt(reader);
				int tsid = readInt(reader);
				int onid = readInt(reader);
				
				out.println("  SID\t= " + sid);
				out.println("  TSID\t= " + tsid);
				out.println("  ONID\t= " + onid);
				out.println(" ");
				
				int eventCount = readInt(reader);
				out.println("   Number of Events = " + eventCount);
				out.println(" ");
				for (int c = 0; c < eventCount; c++) 
				{
					out.println("    Event " + (c+1));
					out.println(" ");

					int eventID = readInt(reader);
			    	double startTime = readDateTime(reader);
					double duration = readDateTime(reader);
			    	byte runningStatus = readByte(reader);
			    	boolean freeToAir = readBoolean(reader);

					out.println("     EventID\t\t= " + eventID);
					out.println("     StartTime\t\t= " + getDateTime(startTime));
					out.println("     EndTime\t\t= " + getDateTime(startTime+duration));
					out.println("     RunningStatus\t= " + runningStatus);
					out.println("     FreeToAir\t\t= " + freeToAir);
					out.println(" ");
					
					boolean shortdescription_valid = readBoolean(reader);
					if (shortdescription_valid)
					{
						out.println("      Short Description");
						out.println(" ");
						out.println("       Name\t\t= " + readString(reader));
						out.println("       Description\t= " + readString(reader));
						out.println("       Language\t\t= " + readString(reader));						
						out.println(" ");
					}
					
					boolean extendeddescription_valid = readBoolean(reader);
					if (extendeddescription_valid)
					{
						out.println("      Extended Description");
						out.println(" ");
						out.println("       Language\t\t= " + readString(reader));
						out.println("       Description\t= " + readString(reader));
						out.println(" ");
						
						int key_value_count = readInt(reader);
						if (key_value_count > 0)
						{
							out.println("       Key/Value Pairs = " + key_value_count);
							out.println(" ");
							
							for (int k = 0; k < key_value_count; k++) 
							{
								out.println("        Key/Value Pair " + (k+1));
								out.println(" ");
								out.println("         Key\t= " + readString(reader));
								out.println("         Value\t= " + readString(reader));
								out.println(" ");								
							}	
						}
					}
					
					// The meaning of the Content Descriptor's Nibbles is defined in EN 300 468
					// It describes the Content as Movie/Drama ...
					int content_count = readInt(reader);
					if (content_count > 0)
					{
						out.println("      Content Count = " + content_count);
						out.println(" ");

						for (int k = 0; k < content_count; k++) 
						{
							out.println("        Content " + (k+1));
							out.println(" ");
							out.println("         Content Nibble 1\t= " + readByte(reader));
							out.println("         Content Nibble 2\t= " + readByte(reader));
							out.println("         User Nibble 1\t= " + readByte(reader));
							out.println("         User Nibble 2\t= " + readByte(reader));
							out.println(" ");							
						}							
					}
					
					// The meaning of the Parental Rating Descriptor is defined in EN 300 468
					//  Rating 		  |		Description
					// ---------------|----------------------------------------
					//  0x00 		  |		 undefined
					//  0x01 to 0x0F  |		 minimum age = rating + 3 years
					//  0x10 to 0xFF  |		 defined by the broadcaster
					int parental_ratings_count = readInt(reader);
					if (parental_ratings_count > 0)
					{
						out.println("      Parental Ratings Count = " + parental_ratings_count);
						out.println(" ");

						for (int k = 0; k < parental_ratings_count; k++) 
						{
							out.println("        Parental Rating " + (k+1));
							out.println(" ");
							out.println("         Country\t= " + readString(reader));
							out.println("         Rating\t= " + readInt(reader));
							out.println(" ");							
						}							
					}
					
					// The meaning of the Component Descriptor is defined in EN 300 468
					// It describes Aspect Ratio, Subtitles ...
					int component_count = readInt(reader);
					if (component_count > 0)
					{
						out.println("      Components Count = " + component_count);
						out.println(" ");

						for (int k = 0; k < component_count; k++) 
						{
							out.println("        Component " + (k+1));
							out.println(" ");
							out.println("         StreamContent\t= " + readByte(reader));
							out.println("         ComponentType\t= " + readByte(reader));
							out.println("         ComponentTag\t= " + readByte(reader));
							out.println("         LanguageCode\t= " + readString(reader));
							out.println("         Description\t= " + readString(reader));
							out.println(" ");				
						}							
					}									
				}
			}
		} 
		catch (Exception e) 
		{
			e.printStackTrace();
			return;
		}
	}
}
