-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Streams.Stream_IO;
package body Tabula.File_IO is

	function Read_File(Name : in String) return String is
		File: Ada.Streams.Stream_IO.File_Type;
	begin
		Ada.Streams.Stream_IO.Open(File, Ada.Streams.Stream_IO.In_File, Name);
		declare
			Size : constant Ada.Streams.Stream_IO.Count := Ada.Streams.Stream_IO.Size(File);
			Result: String(1 .. Natural(Size));
		begin
			String'Read(Ada.Streams.Stream_IO.Stream(File), Result);
			Ada.Streams.Stream_IO.Close(File);
			return Result;
		end;
	end Read_File;
	
	procedure Write_File(Name : in String; Source : in String) is
		File: Ada.Streams.Stream_IO.File_Type;
	begin
		Ada.Streams.Stream_IO.Create(File, Ada.Streams.Stream_IO.Out_File, Name);
		String'Write(Ada.Streams.Stream_IO.Stream(File), Source);
		Ada.Streams.Stream_IO.Close(File);
	end Write_File;

end Tabula.File_IO;
