-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Directories;
with Ada.Hierarchical_File_Names;
with Ada.IO_Exceptions;
with Ada.Streams.Stream_IO;
with Tabula.Users.Load;
with Tabula.Users.Save;
package body Tabula.Users.Lists is
	use type Ada.Strings.Unbounded.Unbounded_String;
	
	procedure Load_Users_Log (List : in out User_List) is
	begin
		if not List.Log_Read then
			begin
				declare
					File : Ada.Streams.Stream_IO.File_Type :=
						Ada.Streams.Stream_IO.Open (
							Ada.Streams.Stream_IO.In_File,
							Name => List.Log_File_Name.all);
				begin
					Users_Log.Map'Read (Ada.Streams.Stream_IO.Stream (File), List.Log);
					Ada.Streams.Stream_IO.Close (File);
				end;
			exception
				when Ada.IO_Exceptions.Name_Error => null;
			end;
			List.Log_Read := True;
		end if;
	end Load_Users_Log;
	
	procedure Add_To_Users_Log (
		List : in out User_List;
		Id : in String;
		Remote_Addr : in String;
		Remote_Host : in String;
		Now : in Ada.Calendar.Time)
	is
		Item : User_Log_Item := (+Id, +Remote_Addr, +Remote_Host);
	begin
		Load_Users_Log (List);
		Users_Log.Include (List.Log, Item, Now);
		declare
			File : Ada.Streams.Stream_IO.File_Type :=
				Ada.Streams.Stream_IO.Create (
					Ada.Streams.Stream_IO.Out_File,
					Name => List.Log_File_Name.all);
		begin
			Users_Log.Map'Write (Ada.Streams.Stream_IO.Stream (File), List.Log);
			Ada.Streams.Stream_IO.Close (File);
		end;
	end Add_To_Users_Log;
	
	Upper_Subdirectory_Name : constant String := "+A";
	
	function User_Full_Name (
		Directory : String;
		Id : String;
		Only_Existing : Boolean := False)
		return String
	is
		Lower_Name : constant String :=
			Ada.Hierarchical_File_Names.Compose (
				Directory => Directory,
				Relative_Name => Id);
	begin
		if Ada.Directories.Exists (Lower_Name) then
			return Lower_Name;
		else
			declare
				Upper_Directory : constant String :=
					Ada.Hierarchical_File_Names.Compose (
						Directory => Directory,
						Relative_Name => Upper_Subdirectory_Name);
				Upper_Name : constant String :=
					Ada.Hierarchical_File_Names.Compose (
						Directory => Upper_Directory,
						Relative_Name => Id);
			begin
				if Ada.Directories.Exists (Upper_Name) then
					return Upper_Name;
				else
					if Only_Existing then
						raise Ada.IO_Exceptions.Name_Error;
					end if;
					if Id (Id'First) in 'A' .. 'Z' then
						if not Ada.Directories.Exists (Upper_Directory) then
							Ada.Directories.Create_Directory (Upper_Directory);
						end if;
						return Upper_Name;
					else
						return Lower_Name;
					end if;
				end if;
			end;
		end if;
	end User_Full_Name;
	
	-- implementation
	
	function Create (
		Directory : not null Static_String_Access;
		Log_File_Name : not null Static_String_Access)
		return User_List is
	begin
		return (
			Directory => Directory,
			Log_File_Name => Log_File_Name,
			Log_Read => False,
			Log => Users_Log.Empty_Map);
	end Create;
	
	function Exists (List : User_List; Id : String) return Boolean is
	begin
		declare
			Dummy_File_Name : constant String :=
				User_Full_Name (List.Directory.all, Id, Only_Existing => True);
		begin
			return True;
		end;
	exception
		when Ada.IO_Exceptions.Name_Error => return False;
	end Exists;
	
	procedure Query (
		List : in out User_List;
		Id : in String;
		Password : in String;
		Remote_Addr : in String;
		Remote_Host : in String;
		Now : in Ada.Calendar.Time;
		Info : out User_Info;
		State : out User_State) is
	begin
		if Id'Length = 0 then
			State := Log_Off;
		elsif not Exists (List, Id) then
			State := Unknown;
		else
			Load (User_Full_Name (List.Directory.all, Id), Info);
			if Info.Password /= Digest (Password) or else not Info.Renamed.Is_Null then
				State := Invalid;
			else
				State := Valid;
				if Id /= Administrator then
					if not Info.No_Log then
						Add_To_Users_Log (
							List,
							Id => Id,
							Remote_Addr => Remote_Addr,
							Remote_Host => Remote_Host,
							Now => Now);
					else
						Add_To_Users_Log (
							List,
							Id => Id,
							Remote_Addr => "",
							Remote_Host => "",
							Now => Now); -- log only time
					end if;
				end if;
			end if;
		end if;
	end Query;
	
	procedure New_User (
		List : in out User_List;
		Id : in String;
		Password : in String;
		Remote_Addr : in String;
		Remote_Host : in String;
		Now : in Ada.Calendar.Time;
		Result : out Boolean) is
	begin
		if Exists (List, Id) then
			Result := False;
		else
			declare
				Info : User_Info := (
					Password => Digest (Password),
					Creation_Remote_Addr => +Remote_Addr,
					Creation_Remote_Host => +Remote_Host,
					Creation_Time => Now,
					Last_Remote_Addr => +Remote_Addr,
					Last_Remote_Host => +Remote_Host,
					Last_Time => Now,
					Ignore_Request => False,
					Disallow_New_Village => False,
					No_Log => False,
					Renamed => Ada.Strings.Unbounded.Null_Unbounded_String);
			begin
				Save (User_Full_Name (List.Directory.all, Id), Info);
				Result := True;
			end;
		end if;
	exception
		when Ada.IO_Exceptions.Name_Error => Result := False;
	end New_User;
	
	procedure Update (
		List : in out User_List;
		Id : in String;
		Remote_Addr : in String;
		Remote_Host : in String;
		Now : in Ada.Calendar.Time;
		Info : in out User_Info) is
	begin
		Info.Last_Remote_Addr := +Remote_Addr;
		Info.Last_Remote_Host := +Remote_Host;
		Info.Last_Time := Now;
		Save (User_Full_Name (List.Directory.all, Id), Info);
	end Update;
	
	function All_Users (List : User_List) return User_Info_Maps.Map is
		procedure Add (Result : in out User_Info_Maps.Map; Directory : in String) is
			Search : aliased Ada.Directories.Search_Type;
		begin
			Ada.Directories.Start_Search (
				Search,
				Directory,
				"*",
				Filter => (Ada.Directories.Ordinary_File => True, others => False));
			while Ada.Directories.More_Entries(Search) loop
				declare
					File : Ada.Directories.Directory_Entry_Type
						renames Ada.Directories.Look_Next_Entry (Search);
					Id : String := Ada.Directories.Simple_Name (File);
				begin
					if Id (Id'First) /= '.' then -- excluding dot file
						declare
							Info : User_Info;
						begin
							Load (User_Full_Name (List.Directory.all, Id), Info);
							User_Info_Maps.Include (Result, Id, Info);
						end;
					end if;
				end;
				Ada.Directories.Skip_Next_Entry (Search);
			end loop;
			Ada.Directories.End_Search(Search);
		end Add;
	begin
		return Result : User_Info_Maps.Map do
			Add (Result, List.Directory.all);
			declare
				Upper_Directory : constant String :=
					Ada.Hierarchical_File_Names.Compose (
						Directory => List.Directory.all,
						Relative_Name => Upper_Subdirectory_Name);
			begin
				if Ada.Directories.Exists (Upper_Directory) then
					Add (Result, Upper_Directory);
				end if;
			end;
		end return;
	end All_Users;
	
	procedure Muramura_Count (
		List : in out User_List;
		Now : Ada.Calendar.Time;
		Muramura_Duration : Duration;
		Result : out Natural)
	is
		Muramura_Set : Users_Log.Map;
	begin
		Load_Users_Log (List);
		for I in List.Log.Iterate loop
			if Now - Users_Log.Element (I) <= Muramura_Duration then
				declare
					Item : User_Log_Item := (Users_Log.Key (I).Id,
						Ada.Strings.Unbounded.Null_Unbounded_String,
						Ada.Strings.Unbounded.Null_Unbounded_String);
				begin
					Users_Log.Include (Muramura_Set, Item, Now);
				end;
			end if;
		end loop;
		Result := Muramura_Set.Length;
	end Muramura_Count;
	
	function "<" (Left, Right : User_Log_Item) return Boolean is
	begin
		if Left.Id < Right.Id then
			return True;
		elsif Left.Id > Right.Id then
			return False;
		elsif Left.Remote_Addr < Right.Remote_Addr then
			return True;
		elsif Left.Remote_Addr > Right.Remote_Addr then
			return False;
		else
			return Left.Remote_Host < Right.Remote_Host;
		end if;
	end "<";
	
	procedure Iterate_Log (
		List : in out User_List;
		Process : not null access procedure (
			Id : in String;
			Remote_Addr : in String;
			Remote_Host : in String;
			Time : in Ada.Calendar.Time)) is
	begin
		Load_Users_Log (List);
		for I in List.Log.Iterate loop
			declare
				Key : User_Log_Item renames Users_Log.Key (I);
			begin
				Process (
					Id => Key.Id.Constant_Reference,
					Remote_Addr => Key.Remote_Addr.Constant_Reference,
					Remote_Host => Key.Remote_Host.Constant_Reference,
					Time => List.Log.Constant_Reference (I));
			end;
		end loop;
	end Iterate_Log;
	
end Tabula.Users.Lists;
