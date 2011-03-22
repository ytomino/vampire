-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Streams.Stream_IO;
with YAML.Streams;
package body Tabula.Villages.Lists is
	use Summary_Maps;
	use User_Lists;
	use type Ada.Strings.Unbounded.Unbounded_String;
	use type YAML.Event_Type;
	
	function Get_YAML_Type (Name : String) return String is
		Result : Ada.Strings.Unbounded.Unbounded_String;
		File : Ada.Streams.Stream_IO.File_Type :=
			Ada.Streams.Stream_IO.Open (Ada.Streams.Stream_IO.In_File, Name => Name);
		Parser : YAML.Parser := YAML.Streams.Create (
			Ada.Streams.Stream_IO.Stream (File));
	begin
		declare
			procedure Process (Event : in YAML.Event; Start_Mark, End_Mark : in YAML.Mark) is
			begin
				if Event.Event_Type /= YAML.Stream_Start then
					raise Ada.IO_Exceptions.Data_Error;
				end if;
			end Process;
		begin
			YAML.Parse (Parser, Process'Access);
		end;
		declare
			procedure Process (Event : in YAML.Event; Start_Mark, End_Mark : in YAML.Mark) is
			begin
				if Event.Event_Type /= YAML.Document_Start then
					raise Ada.IO_Exceptions.Data_Error;
				end if;
			end Process;
		begin
			YAML.Parse (Parser, Process'Access);
		end;
		declare
			procedure Process (Event : in YAML.Event; Start_Mark, End_Mark : in YAML.Mark) is
			begin
				if Event.Event_Type /= YAML.Mapping_Start then
					raise Ada.IO_Exceptions.Data_Error;
				end if;
				Result := +Event.Tag.all;
			end Process;
		begin
			YAML.Parse (Parser, Process'Access);
		end;
		Ada.Streams.Stream_IO.Close (File);
		if Result.Element (1) = '!' then
			Ada.Strings.Unbounded.Delete (Result, 1, 1);
		end if;
		return Ada.Strings.Unbounded.To_String (Result);
	end Get_YAML_Type;
	
	function Get_Type_Index (List : Village_List; Type_Code : String) return Positive is
	begin
		for I in 1 .. List.Registered_Type_Count loop
			if List.Registered_Types (I).Type_Code.all = Type_Code then
				return I;
			end if;
		end loop;
		raise Ada.IO_Exceptions.Data_Error with "unknown type " & Type_Code;
	end Get_Type_Index;
	
	procedure Cache_Summaries (List : in Village_List) is
		File: Ada.Streams.Stream_IO.File_Type :=
			Ada.Streams.Stream_IO.Create (Ada.Streams.Stream_IO.Out_File, List.Cache_File_Name.all);
	begin
		Summary_Maps.Map'Write (Ada.Streams.Stream_IO.Stream (File), List.Map);
		Ada.Streams.Stream_IO.Close (File);
	end Cache_Summaries;
	
	procedure Read_Summaries (List : in out Village_List; Update_Cache : Boolean) is
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
									Type_Code : constant String := Get_YAML_Type (Ada.Directories.Full_Name (File));
									Type_Index : constant Positive := Get_Type_Index (List, Type_Code);
									Summary : Village_Summary
										renames List.Registered_Types (Type_Index).Load_Summary (List, Id);
								begin
									Insert (List.Map, Id, Summary);
								end;
							end if;
						end;
					end loop;
					Ada.Directories.End_Search (Search);
					if Update_Cache then
						Cache_Summaries (List);
					end if;
				end;
			end if;
			List.Map_Read := True;
		end if;
	end Read_Summaries;
	
	function Summary (Type_Code : String; Village : Village_Type'Class) return Village_Summary is
		State : Village_State;
		Today : Natural;
	begin
		Get_State (Village, State, Today);
		return Result : Village_Summary := (
			Type_Code => +Type_Code,
			Name => Village.Name,
			By => Village.By,
			Term => Village.Term,
			Today => Today,
			State => State,
			People => Empty_List)
		do
			declare
				procedure Process (Index : Person_Index; Item : Person_Type'Class) is
				begin
				   Append (Result.People, Item.Id.Constant_Reference.Element.all);
				end Process;
			begin
				Iterate_People (Village, Process'Access);
			end;
		end return;
	end Summary;
	
	function Create (
		Data_Directory : not null Static_String_Access;
		HTML_Directory : not null Static_String_Access;
		Blocking_Short_Term_File_Name : not null Static_String_Access;
		Cache_File_Name : not null Static_String_Access;
		Create_Index : not null Create_Index_Procedure;
		Types : Registered_Type_Array)
		return Village_List is
	begin
		return (
			Data_Directory => Data_Directory,
			HTML_Directory => HTML_Directory,
			Blocking_Short_Term_File_Name => Blocking_Short_Term_File_Name,
			Cache_File_Name => Cache_File_Name,
			Create_Index => Create_Index,
			Map => Empty_Map,
			Map_Read => False,
			Registered_Type_Count => Types'Length,
			Registered_Types => Types &
				Registered_Type_Array'(1 .. Registered_Type_Capacity - Types'Length => <>));
	end Create;
	
	function File_Name (List : Village_List; Id : Village_Id) return String is
	begin
		return Ada.Directories.Compose (List.Data_Directory.all, Id);
	end File_Name;
	
	function HTML_File_Name (List : Village_List; Id : Village_Id; Day : Natural) return String is
	begin
		return Ada.Directories.Compose (
			Containing_Directory => List.HTML_Directory.all,
			Name => Id & "-" & Image (Day),
			Extension => "html");
	end HTML_File_Name;
	
	function Exists (List : Village_List; Id : Village_Id) return Boolean is
	begin
		return Ada.Directories.Exists (File_Name (List, Id));
	end Exists;
	
	function New_Village_Id (List : Village_List) return Village_Id is
		Next : Integer := 0;
	begin
		declare
			Search : Ada.Directories.Search_Type;
		begin
			Ada.Directories.Start_Search (Search, List.Data_Directory.all, "????");
			while Ada.Directories.More_Entries (Search) loop
				declare
					File : Ada.Directories.Directory_Entry_Type;
				begin
					Ada.Directories.Get_Next_Entry (Search, File);
					declare
						File_Name : constant String := Ada.Directories.Simple_Name (File);
					begin
						declare
							Num : constant Integer := Integer'Value (File_Name);
						begin
							if Num >= Next then
								Next := Num + 1;
							end if;
						end;
					exception
						when Constraint_Error => null;
					end;
				end;
			end loop;
			Ada.Directories.End_Search (Search);
		end;
		declare
			Image : String := Integer'Image (Next);
			Result : Village_Id;
		begin
			if Image (Image'First) = ' ' then
				Image (Image'First) := '0';
			end if;
			Result := String'(1 .. 4 - Image'Length => '0') & Image;
			return Result;
		end;
	end New_Village_Id;
	
	procedure Get_Summaries (List : in out Village_List; Result : out Summary_Maps.Map) is
	begin
		Read_Summaries (List, True);
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
				if not Long_Only or else V.Term = Long then
					if Filter (V.State) then
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
		List : in out Village_List;
		Id : Village_Id;
		Summary : Village_Summary) is
	begin
		Read_Summaries (List, False);
		Include (List.Map, Id, Summary);
		if Summary.State = Closed
			and then not Ada.Directories.Exists (HTML_File_Name (List, Id, 0))
		then
			declare
				Type_Index : constant Positive :=
					Get_Type_Index (List, Summary.Type_Code.Constant_Reference.Element.all);
			begin
				List.Registered_Types (Type_Index).Create_Log (List, Id);
			end;
			List.Create_Index (List.Map, Update => True);
		end if;
		Cache_Summaries (List);
	end Update;
	
	procedure Refresh (List : in out Village_List) is
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
		Read_Summaries (List, True);
		-- remake html
		declare
			I : Summary_Maps.Cursor := List.Map.First;
		begin
			while Has_Element (I) loop
				declare
					Id : constant String := List.Map.Constant_Reference (I).Key.all;
					Type_Code : constant String := Get_YAML_Type (File_Name (List, Id));
					Type_Index : constant Positive := Get_Type_Index (List, Type_Code);
				begin
					List.Registered_Types (Type_Index).Create_Log (List, Id);
				end;
				Next (I);
			end loop;
		end;
		-- remake index
		List.Create_Index (List.Map, Update => True);
	end Refresh;
	
	function Blocking_Short_Term (List : Village_List) return Boolean is
	begin
		return Ada.Directories.Exists (List.Blocking_Short_Term_File_Name.all);
	end Blocking_Short_Term;
	
end Tabula.Villages.Lists;
