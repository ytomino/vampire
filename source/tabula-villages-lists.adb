-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Directories;
with Ada.Formatting;
with Ada.Streams.Stream_IO;
package body Tabula.Villages.Lists is
	use Summary_Maps;
	use type Ada.Strings.Unbounded.Unbounded_String;
	
	procedure Read_Summaries (List : in out Villages_List) is
	begin
		if not List.Map_Read then
			if Ada.Directories.Exists (List.Cache_File_Name.all) then
				declare
					Cache_File : Ada.Streams.Stream_IO.File_Type :=
						Ada.Streams.Stream_IO.Open (Ada.Streams.Stream_IO.In_File, List.Cache_File_Name.all);
				begin
					Summary_Maps.Map'Read (Ada.Streams.Stream_IO.Stream (Cache_File), List.Map);
					Ada.Streams.Stream_IO.Close (Cache_File);
				end;
			else
				declare
					Search : Ada.Directories.Search_Type;
					File : Ada.Directories.Directory_Entry_Type;
				begin
					Ada.Directories.Start_Search (Search, List.Data_Directory.all, "????");
					while Ada.Directories.More_Entries (Search) loop
						Ada.Directories.Get_Next_Entry (Search, File);
						declare
							Id : String renames Ada.Directories.Simple_Name (File);
						begin
							if Id (Id'First) in '0' .. '9' then
								declare
									Summary : Village_Summary renames List.Load_Summary (List, Id);
								begin
									Insert (List.Map, Id, Summary);
								end;
							end if;
						end;
					end loop;
					Ada.Directories.End_Search (Search);
					-- cache
					declare
						File: Ada.Streams.Stream_IO.File_Type :=
							Ada.Streams.Stream_IO.Create (Ada.Streams.Stream_IO.Out_File, List.Cache_File_Name.all);
					begin
						Summary_Maps.Map'Write (Ada.Streams.Stream_IO.Stream (File), List.Map);
						Ada.Streams.Stream_IO.Close (File);
					end;
				end;
			end if;
			List.Map_Read := True;
		end if;
	end Read_Summaries;
	
	function Create (
		Data_Directory : not null Static_String_Access;
		HTML_Directory : not null Static_String_Access;
		Blocking_Short_Term_File_Name : not null Static_String_Access;
		Cache_File_Name : not null Static_String_Access;
		Load_Summary : not null Load_Summary_Function;
		Create_Log : not null Create_Log_Procedure;
		Create_Index : not null Create_Index_Procedure)
		return Villages_List is
	begin
		return (
			Data_Directory => Data_Directory,
			HTML_Directory => HTML_Directory,
			Blocking_Short_Term_File_Name => Blocking_Short_Term_File_Name,
			Cache_File_Name => Cache_File_Name,
			Load_Summary => Load_Summary,
			Create_Log => Create_Log,
			Create_Index => Create_Index,
			Map => Empty_Map,
			Map_Read => False);
	end Create;
	
	function File_Name (List : Villages_List; Id : Village_Id) return String is
	begin
		return Ada.Directories.Compose (List.Data_Directory.all, Id);
	end File_Name;
	
	function HTML_File_Name (List : Villages_List; Id : Village_Id; Day : Natural) return String is
		function To_String is new Ada.Formatting.Integer_Image (
			Natural,
			Zero_Sign => Ada.Formatting.None,
			Plus_Sign => Ada.Formatting.None);
	begin
		return Ada.Directories.Compose (
			Containing_Directory => List.HTML_Directory.all,
			Name => Id & "-" & To_String (Day),
			Extension => "html");
	end HTML_File_Name;
	
	function Exists (List : Villages_List; Id : Village_Id) return Boolean is
	begin
		return Ada.Directories.Exists (File_Name (List, Id));
	end Exists;
	
	function New_Village_Id (List : Villages_List) return Village_Id is
		Search : Ada.Directories.Search_Type;
		File : Ada.Directories.Directory_Entry_Type;
		Result : Integer := 0;
	begin
		Ada.Directories.Start_Search (Search, List.Data_Directory.all, "????");
		while Ada.Directories.More_Entries (Search) loop
			Ada.Directories.Get_Next_Entry (Search, File);
			declare
				File_Name : String renames Ada.Directories.Simple_Name (File);
			begin
				declare
					Num : constant Integer := Integer'Value (File_Name);
				begin
					if Num >= Result then
						Result := Num + 1;
					end if;
				end;
			exception
				when Constraint_Error => null;
			end;
		end loop;
		Ada.Directories.End_Search (Search);
		declare
			Image : String := Integer'Image (Result);
		begin
			if Image (Image'First) = ' ' then
				Image (Image'First) := '0';
			end if;
			return (1 .. 4 - Image'Length => '0') & Image;
		end;
	end New_Village_Id;
	
	procedure Get_Summaries (List : in out Villages_List; Result : out Summary_Maps.Map) is
	begin
		Read_Summaries (List);
		List.Create_Index (List.Map, Update => False);
		Result := List.Map;
	end Get_Summaries;
	
	function Exists_Opened_By (
		Summaries : Summary_Maps.Map;
		User_Id : String;
		Excluding : Village_Id := Invalid_Village_Id)
		return Boolean
	is
		I : Summary_Maps.Cursor := Summaries.First;
	begin
		while Has_Element (I) loop
			declare
				V : Village_Summary renames Summaries.Constant_Reference (I).Element.all;
			begin
				if V.State <= Playing and then V.By = User_Id
					and then Summaries.Constant_Reference (I).Key.all /= Excluding
				then
					return True;
				end if;
			end;
			Next (I);
		end loop;
		return False;
	end Exists_Opened_By;

	function Count_Joined_By (
		Summaries : Summary_Maps.Map;
		User_Id : String;
		Filter : Village_State_Set;
		Long_Only : Boolean := False;
		Including_Escaped : Boolean := False) -- unimplemented
		return Natural
	is
		Result : Natural := 0;
		I : Summary_Maps.Cursor := Summaries.First;
	begin
		while Has_Element (I) loop
			declare
				V : Village_Summary renames Summaries.Constant_Reference(I).Element.all;
			begin
				if not Long_Only or else V.Day_Duration >= 24 * 60 * 60.0 then
					if V.State <= Playing then
						if V.People.Contains (User_Id) then
							Result := Result + 1;
						end if;
					end if;
				end if;
			end;
			Next (I);
		end loop;
		return Result;
	end Count_Joined_By;

	procedure Update (
		List : in out Villages_List;
		Id : Village_Id;
		Summary : Village_Summary) is
	begin
		Read_Summaries (List);
		Include (List.Map, Id, Summary);
		if Summary.State = Closed
			and then not Ada.Directories.Exists (HTML_File_Name (List, Id, 0))
		then
			List.Create_Log (List, Id);
			List.Create_Index (List.Map, Update => True);
		end if;
	end Update;
	
	procedure Refresh (List : in out Villages_List) is
		Search : Ada.Directories.Search_Type;
		File : Ada.Directories.Directory_Entry_Type;
	begin
		-- delete cache
		if Ada.Directories.Exists (List.Cache_File_Name.all) then
			Ada.Directories.Delete_File (List.Cache_File_Name.all);
		end if;
		List.Map_Read := False;
		-- delete html
		Ada.Directories.Start_Search (Search, List.HTML_Directory.all, "*.html");
		while Ada.Directories.More_Entries (Search) loop
			Ada.Directories.Get_Next_Entry (Search, File);
			declare
				File_Name : constant String := Ada.Directories.Full_Name (File);
			begin
				Ada.Directories.Delete_File (File_Name);
			end;
		end loop;
		Ada.Directories.End_Search (Search);
		-- remake cache
		Read_Summaries (List);
		-- remake html
		declare
			I : Summary_Maps.Cursor := List.Map.First;
		begin
			while Has_Element (I) loop
				List.Create_Log (List, List.Map.Constant_Reference (I).Key.all);
				Next (I);
			end loop;
		end;
		-- remake index
		List.Create_Index (List.Map, Update => True);
	end Refresh;
	
	function Blocking_Short_Term (List : Villages_List) return Boolean is
	begin
		return Ada.Directories.Exists (List.Blocking_Short_Term_File_Name.all);
	end Blocking_Short_Term;
	
end Tabula.Villages.Lists;
