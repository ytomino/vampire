-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Web.RSS;
with Tabula.Configurations;
with Tabula.Configurations.Templates;
with Tabula.Renderers.Log;
package body Tabula.Villages.Lists is
	use type Ada.Strings.Unbounded.Unbounded_String;
	
	function "+" (S : Ada.Strings.Unbounded.Unbounded_String) return String renames Ada.Strings.Unbounded.To_String;
	-- function "+" (S : String) return Ada.Strings.Unbounded.Unbounded_String renames Ada.Strings.Unbounded.To_Unbounded_String;
	
	package Sorting is new Village_Lists.Generic_Sorting;
	
	-- bodies
	
	function Exists (Id : Village_Id) return Boolean is
		File_Name : String renames Ada.Directories.Compose (Configurations.Villages_Data_Directory, Id);
	begin
		return Ada.Directories.Exists (File_Name);
	end Exists;
	
	function New_Village_Id return Village_Id is
		Search : Ada.Directories.Search_Type;
		File : Ada.Directories.Directory_Entry_Type;
		Result : Integer := 0;
	begin
		Ada.Directories.Start_Search(Search, Configurations.Villages_Data_Directory, "????");
		while Ada.Directories.More_Entries(Search) loop
			Ada.Directories.Get_Next_Entry(Search, File);
			declare
				File_Name : String renames Ada.Directories.Simple_Name(File);
			begin
				declare
					Num : constant Integer := Integer'Value(File_Name);
				begin
					if Num >= Result then
						Result := Num + 1;
					end if;
				end;
			exception
				when Constraint_Error => null;
			end;
		end loop;
		Ada.Directories.End_Search(Search);
		declare
			Image : String := Integer'Image(Result);
		begin
			if Image (Image'First) = ' ' then
				Image (Image'First) := '0';
			end if;
			return (1 .. 4 - Image'Length => '0') & Image;
		end;
	end New_Village_Id;
	
	function "<" (L, R : Village_List_Item) return Boolean is
	begin
		return L.Id < R.Id;
	end "<";
	
	function Joined (
		User_Id : String;
		List : Village_Lists.Vector;
		Long_Only : Boolean) return Boolean is
	begin
		for I in List.First_Index .. List.Last_Index loop
			declare
				V : Village_List_Item renames List.Constant_Reference(I).Element.all;
			begin
				if not Long_Only or else V.Day_Duration >= 24 * 60 * 60.0 then
					if V.State <= Opened then
						declare
							J : String_Lists.Cursor := V.People.First;
						begin
							while String_Lists.Has_Element (J) loop
								if String_Lists.Element (J) = User_Id then
									return True;
								end if;
								String_Lists.Next(J);
							end loop;
						end;
					end if;
				end if;
			end;
		end loop;
		return False;
	end Joined;
	
	function Created (
		User_Id : String;
		List : Village_Lists.Vector;
		Excluding : Village_Id) return Boolean is
	begin
		for I in List.First_Index .. List.Last_Index loop
			declare
				V : Village_List_Item renames List.Constant_Reference (I).Element.all;
			begin
				if V.State <= Opened and then V.By = User_Id
					and then V.Id /= Excluding
				then
					return True;
				end if;
			end;
		end loop;
		return False;
	end Created;
	
	procedure Make_Log_Index (List : not null access constant Lists.Village_Lists.Vector) is
		Renderer : Renderers.Log.Renderer := Renderers.Log.Renderer'(Configuration => Configurations.Templates.Configuration);
		File: Ada.Streams.Stream_IO.File_Type;
	begin
		Ada.Streams.Stream_IO.Create (File, Name => Configurations.List_HTML_File_Name);
		begin
			Renderer.List_Page(Ada.Streams.Stream_IO.Stream (File), List.all);
		exception
			when others =>
				Ada.Streams.Stream_IO.Close (File);
				raise;
		end;
		Ada.Streams.Stream_IO.Close (File);
	end Make_Log_Index;
	
	procedure Make_RSS (List : not null access constant Lists.Village_Lists.Vector) is
		File: Ada.Streams.Stream_IO.File_Type;
	begin
		Ada.Streams.Stream_IO.Create (File, Ada.Streams.Stream_IO.Out_File, Configurations.List_RSS_File_Name);
		declare
			Stream : not null access Ada.Streams.Root_Stream_Type'Class := Ada.Streams.Stream_IO.Stream (File);
		begin
			Web.RSS.RSS_Start (Stream,
				Title => "参加募集中の村 - The Village of Vampire",
				Description => "",
				Link => "../");
			for I in reverse List.First_Index .. List.Last_Index loop
				declare
					Item : Village_List_Item renames List.Constant_Reference (I).Element.all;
				begin
					if Item.State = Prologue then
						Web.RSS.RSS_Item (Stream,
							Title => +Item.Name,
							Description => "",
							Link => "../?village=" & Item.Id);
					end if;
				end;
			end loop;
			Web.RSS.RSS_End (Stream);
		exception
			when others =>
				Ada.Streams.Stream_IO.Close (File);
				raise;
		end;
		Ada.Streams.Stream_IO.Close (File);
	end Make_RSS;
	
	function Village_List (
		Load_Info : not null access function (Id : in Village_Id) return Village_List_Item)
		return Village_Lists.Vector is
	begin
		return Result : aliased Village_Lists.Vector do
			if Ada.Directories.Exists(Configurations.Village_List_Cache_File_Name) then
				declare
					Cache_File : Ada.Streams.Stream_IO.File_Type;
				begin
					Ada.Streams.Stream_IO.Open(Cache_File, Ada.Streams.Stream_IO.In_File, Configurations.Village_List_Cache_File_Name);
					begin
						Village_Lists.Vector'Read(Ada.Streams.Stream_IO.Stream(Cache_File), Result);
						Ada.Streams.Stream_IO.Close(Cache_File);
					exception
						when others =>
							Ada.Streams.Stream_IO.Close(Cache_File);
							raise;
					end;
				end;
			else
				declare
					Search : Ada.Directories.Search_Type;
					File : Ada.Directories.Directory_Entry_Type;
				begin
					Ada.Directories.Start_Search(Search, Configurations.Villages_Data_Directory, "????");
					while Ada.Directories.More_Entries(Search) loop
						Ada.Directories.Get_Next_Entry(Search, File);
						declare
							Id : String renames Ada.Directories.Simple_Name(File);
						begin
							if Id (Id'First) in '0' .. '9' then
								declare
									Info : Village_List_Item renames Load_Info (Id);
								begin
									Village_Lists.Append(Result, Info);
								end;
							end if;
						end;
					end loop;
					Ada.Directories.End_Search(Search);
					Sorting.Sort(Result);
					if not Ada.Directories.Exists (Configurations.List_HTML_File_Name) then
						Make_Log_Index (Result'Access);
					end if;
					if not Ada.Directories.Exists (Configurations.List_RSS_File_Name) then
						Make_RSS (Result'Access);
					end if;
					-- cache
					declare
						File: Ada.Streams.Stream_IO.File_Type;
					begin
						Ada.Streams.Stream_IO.Create(File, Ada.Streams.Stream_IO.Out_File, Configurations.Village_List_Cache_File_Name);
						Village_Lists.Vector'Write(Ada.Streams.Stream_IO.Stream(File), Result);
						Ada.Streams.Stream_IO.Close(File);
					exception
						when others =>
							Ada.Streams.Stream_IO.Close(File);
							raise;
					end;
				end;
			end if;
		end return;
	end Village_List;
	
	procedure Update_Village_List (
		Remake_All : Boolean := False;
		Load_Info : not null access function (Id : in Village_Id) return Village_List_Item;
		Create_Log : not null access procedure (Id : in Village_Id)) is
	begin
		if Remake_All then
			declare
				Search : Ada.Directories.Search_Type;
				File : Ada.Directories.Directory_Entry_Type;
			begin
				Ada.Directories.Start_Search(Search, Configurations.Villages_HTML_Directory, "*.html");
				while Ada.Directories.More_Entries(Search) loop
					Ada.Directories.Get_Next_Entry(Search, File);
					declare
						File_Name : String renames Ada.Directories.Full_Name(File);
					begin
						Ada.Directories.Delete_File(File_Name);
					end;
				end loop;
				Ada.Directories.End_Search(Search);
			end;
		end if;
		begin
			Ada.Directories.Delete_File(Configurations.Village_List_Cache_File_Name);
		exception
			when Ada.Directories.Name_Error => null;
		end;
		declare
			Result : aliased constant Village_Lists.Vector := Village_List (Load_Info);
			Log_Created : Boolean := False;
		begin
			for I in Result.First_Index .. Result.Last_Index loop
				declare
					Item : Village_List_Item renames Result.Constant_Reference (I).Element.all;
				begin
					if Item.State = Closed then
						declare
							Log : String renames Ada.Directories.Compose (
								Configurations.Villages_HTML_Directory,
								Item.Id & "-0.html");
						begin
							if not Ada.Directories.Exists(Log) then
								Create_Log (Item.Id);
								Log_Created := True;
							end if;
						end;
					end if;
				end;
			end loop;
			if Remake_All or else Log_Created then
				Make_Log_Index (Result'Access);
			end if;
			Make_RSS (Result'Access);
		end;
	end Update_Village_List;
	
	function Short_Term_Village_Blocking return Boolean is
	begin
		return Ada.Directories.Exists (Configurations.Short_Term_Village_Blocking_File_Name);
	end Short_Term_Village_Blocking;
	
end Tabula.Villages.Lists;
