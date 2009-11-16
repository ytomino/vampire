-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Strings.Unbounded;
procedure Tabula.Renderers.Users_Page (
	Object : in Renderer'Class;
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Village_List : in Villages.Lists.Village_Lists.Vector;
	User_List : in Users.Lists.User_Info_Maps.Map;
	User_Id : in String;
	User_Password : in String)
is
	function "+" (S : Ada.Strings.Unbounded.Unbounded_String) return String renames Ada.Strings.Unbounded.To_String;
	
	procedure Handle (
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Tag : in String;
		Template : in Web.Producers.Template) is
	begin
		if Tag = "back" then
			Write (Output, "<a href=");
			Link (Object, Output, User_Id => User_Id, User_Password => User_Password);
			Write (Output, '>');
			Web.Producers.Produce (Output, Template);
			Write (Output, "</a>");
		elsif Tag = "user" then
			declare
				I : Users.Lists.User_Info_Maps.Cursor := User_List.First;
				procedure Handle_User (
					Output : not null access Ada.Streams.Root_Stream_Type'Class;
					Tag : in String;
					Template : in Web.Producers.Template) is
				begin
					if Tag = "id" then
						Web.Write_In_HTML (
							Output,
							Object.HTML_Version,
							Users.Lists.User_Info_Maps.Key (I));
					elsif Tag = "joined_count" then
						Web.Write_In_HTML (
							Output,
							Object.HTML_Version,
							Natural'Image (Villages.Lists.Closed_Only_Joined_Count (Users.Lists.User_Info_Maps.Key (I), Village_List, Escaped => False)));
					elsif Tag = "renamed" then
						Web.Write_In_HTML (
							Output,
							Object.HTML_Version,
							+User_List.Constant_Reference (I).Element.Renamed);
					else
						Handle (Output, Tag, Template);
					end if;
				end Handle_User;
			begin
				while Users.Lists.User_Info_Maps.Has_Element (I) loop
					if Villages.Lists.Closed_Only_Joined_Count (Users.Lists.User_Info_Maps.Key (I), Village_List, Escaped => True) > 0 then
						Web.Producers.Produce (Output, Template, Handler => Handle_User'Access);
					end if;
					Users.Lists.User_Info_Maps.Next (I);
				end loop;
			end;
		else
			raise Program_Error with "Invalid template """ & Tag & """";
		end if;
	end Handle;
begin
	Produce (Object, Output, Object.Configuration.Template_Users_File_Name.all, Handle'Access);
end Tabula.Renderers.Users_Page;
