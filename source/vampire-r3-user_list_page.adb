-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Directories;
procedure Vampire.R3.User_List_Page (
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Form : in Forms.Root_Form_Type'Class;
	Template : in String;
	HTML_Directory : in String;
	Summaries : in Tabula.Villages.Lists.Summary_Maps.Map;
	User_List : in Users.Lists.User_Info_Maps.Map;
	User_Id : in String;
	User_Password : in String)
is
	procedure Handle (
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Tag : in String;
		Template : in Web.Producers.Template) is
	begin
		if Tag = "index" then
			Forms.Write_Link_To_Index_Page (
				Output,
				Form,
				Current_Directory => ".",
				User_Id => User_Id,
				User_Password => User_Password);
		elsif Tag = "logindex" then
			Forms.Write_Link_To_Resource (
				Output,
				Form,
				Current_Directory => ".",
				Resource => Ada.Directories.Compose (
					Containing_Directory => HTML_Directory,
					Name => ""));
		elsif Tag = "user" then
			declare
				I : Users.Lists.User_Info_Maps.Cursor := User_List.First;
				procedure Handle_User (
					Output : not null access Ada.Streams.Root_Stream_Type'Class;
					Tag : in String;
					Template : in Web.Producers.Template) is
				begin
					if Tag = "id" then
						Forms.Write_In_HTML (
							Output,
							Form,
							Users.Lists.User_Info_Maps.Key (I));
					elsif Tag = "joined_count" then
						Forms.Write_In_HTML (
							Output,
							Form,
							Image (
								Tabula.Villages.Lists.Count_Joined_By (
									Summaries,
									Users.Lists.User_Info_Maps.Key (I),
									Filter => (
										Tabula.Villages.Prologue | Tabula.Villages.Playing => False,
										Tabula.Villages.Epilogue | Tabula.Villages.Closed => True),
									Including_Escaped => False)));
					elsif Tag = "renamed" then
						Forms.Write_In_HTML (
							Output,
							Form,
							User_List.Constant_Reference (I).Element.
								Renamed.Constant_Reference.Element.all);
					else
						Handle (Output, Tag, Template);
					end if;
				end Handle_User;
			begin
				while Users.Lists.User_Info_Maps.Has_Element (I) loop
					if Tabula.Villages.Lists.Count_Joined_By (
						Summaries,
						User_List.Constant_Reference (I).Key.all,
						Filter => (
							Tabula.Villages.Prologue | Tabula.Villages.Playing => False,
							Tabula.Villages.Epilogue | Tabula.Villages.Closed => True),
						Including_Escaped => True) > 0
					then
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
	Produce (Output, Template, Handler => Handle'Access);
end Vampire.R3.User_List_Page;