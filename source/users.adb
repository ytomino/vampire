-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Web;
with Tabula; use Tabula;
with Tabula.Users;
with Tabula.Users.Load;
with Tabula.Villages;
with Tabula.Villages.Lists;
with Tabula.Villages.Load;
use type Tabula.Users.User_Info;
procedure Users is
	function "+" (S : Ada.Strings.Unbounded.Unbounded_String) return String renames Ada.Strings.Unbounded.To_String;
	package SS is new Ada.Containers.Indefinite_Ordered_Maps(String, Tabula.Users.User_Info);
	use SS;
	Used_Users, Unused_Users : SS.Map;
	Search : Ada.Directories.Search_Type;
	File : Ada.Directories.Directory_Entry_Type;
begin
	Web.Header(Ada.Text_IO.Text_Streams.Stream(Standard_Output), Web.Text); 
	Ada.Text_IO.New_Line;
	Ada.Directories.Start_Search(Search, "./users", "*", (Ordinary_File => True, others => False));
	while Ada.Directories.More_Entries(Search) loop
		Ada.Directories.Get_Next_Entry(Search, File);
		declare
			User_Id : String := Ada.Directories.Simple_Name(File);
		begin
			if User_Id(1) /= '.' then
				declare
					User_Info : Tabula.Users.User_Info;
				begin
					Tabula.Users.Load(User_Id, User_Info);
					Include(Unused_Users, User_Id, User_Info);
				end;
			end if;
		end;
	end loop;
	Ada.Directories.End_Search(Search);
	Ada.Directories.Start_Search(Search, "./villages/data", "????", (Ordinary_File => True, others => False));
	while Ada.Directories.More_Entries(Search) loop
		Ada.Directories.Get_Next_Entry(Search, File);
		if Ada.Directories.Simple_Name(File)(1) /= '.' then
			declare
				Village : Villages.Village_Type;
				Village_Id : Villages.Lists.Village_Id := Ada.Directories.Simple_Name(File);
			begin
				Villages.Load(Village_Id, Village, Info_Only => True);
				for I in Village.People.First_Index .. Village.People.Last_Index loop
					declare
						Id : String := +Village.People.Constant_Reference(I).Element.Id;
					begin
						if Contains(Unused_Users, Id) then
							Include(Used_Users, Id, Element(Unused_Users, Id));
							Exclude(Unused_Users, Id);
						end if;
					end;
				end loop;
				for I in Village.Escaped_People.First_Index .. Village.Escaped_People.Last_Index loop
					declare
						Id : String := +Village.Escaped_People.Constant_Reference(I).Element.Id;
					begin
						if Contains(Unused_Users, Id) then
							Include(Used_Users, Id, Element(Unused_Users, Id));
							Exclude(Unused_Users, Id);
						end if;
					end;
				end loop;
			end;
		end if;
	end loop;
	Ada.Directories.End_Search(Search);
	Exclude(Unused_Users, Tabula.Users.Administrator);
	declare
		I : SS.Cursor := First(Used_Users);
	begin
		while Has_Element(I) loop
			Put(Key(I));
			if Element(I).Renamed /= Null_Unbounded_String then
				Put(" -> ");
				Put(+Element(I).Renamed);
			end if;
			New_Line;
			Next(I);
		end loop;
	end;
	New_Line;
	declare
		I : SS.Cursor := First(Unused_Users);
	begin
		while Has_Element(I) loop
			Put(Key(I));
			if Element(I).Renamed /= Null_Unbounded_String then
				Put(" -> ");
				Put(+Element(I).Renamed);
			end if;
			New_Line;
			Next(I);
		end loop;
	end;
end Users;
