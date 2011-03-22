-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Calendar;
with Ada.Directories;
with Ada.Streams.Stream_IO;
package body Vampire.R3 is
	use type Ada.Calendar.Time;
	use type Tabula.Villages.Village_State;
	
	procedure Produce (
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Template_Source : in String;
		Template_Cache : in String := "";
		Handler : not null access procedure (
			Output : not null access Ada.Streams.Root_Stream_Type'Class;
			Tag : in String;
			Contents : Web.Producers.Template))
	is
		File : Ada.Streams.Stream_IO.File_Type :=
			Ada.Streams.Stream_IO.Open (Ada.Streams.Stream_IO.In_File, Template_Source);
		Template : Web.Producers.Template := Web.Producers.Read (
			Ada.Streams.Stream_IO.Stream (File),
			Ada.Streams.Stream_IO.Size(File),
			Parsing => False);
	begin
		if Template_Cache'Length > 0 then
			if Ada.Directories.Exists (Template_Cache)
				and then Ada.Directories.Modification_Time (Template_Cache) >
					Ada.Directories.Modification_Time (Template_Source)
			then
				-- read parsed-structure from cache file
				declare
					Cache_File : Ada.Streams.Stream_IO.File_Type;
				begin
					Ada.Streams.Stream_IO.Open (
						Cache_File,
						Ada.Streams.Stream_IO.In_File,
						Name => Template_Cache);
					Web.Producers.Read_Parsed_Information (
						Ada.Streams.Stream_IO.Stream (Cache_File),
						Template);
					Ada.Streams.Stream_IO.Close (Cache_File);
				end;
			else
				Web.Producers.Parse (Template);
				-- save parsed-structure to cache file
				declare
					Cache_File : Ada.Streams.Stream_IO.File_Type;
				begin
					Ada.Streams.Stream_IO.Create (
						Cache_File,
						Ada.Streams.Stream_IO.Out_File,
						Name => Template_Cache);
					Web.Producers.Write_Parsed_Information (
						Ada.Streams.Stream_IO.Stream (Cache_File),
						Template);
					Ada.Streams.Stream_IO.Close (Cache_File);
				end;
			end if;
		else
			Web.Producers.Parse (Template);
		end if;
		Web.Producers.Produce (Output, Template, Handler => Handler);
		Ada.Streams.Stream_IO.Close(File);
	end Produce;
	
	function Day_Name (
		Day : Natural;
		Today : Natural;
		State : Tabula.Villages.Village_State)
		return String is
	begin
		if Day = 0 then
			return "プロローグ";
		elsif State >= Tabula.Villages.Epilogue and then Today = Day then
			return "エピローグ";
		else
			return Image (Day) & "日目";
		end if;
	end Day_Name;
	
end Vampire.R3;
