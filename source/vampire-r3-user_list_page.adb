-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Hierarchical_File_Names;
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
	use Users.Lists.User_Info_Maps;
	procedure Handle (
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Tag : in String;
		Template : in Web.Producers.Template) is
	begin
		if Tag = "href_index" then
			Forms.Write_Attribute_Name (Output, "href");
			Forms.Write_Link (
				Output,
				Form,
				Current_Directory => ".",
				Resource => Forms.Self,
				Parameters => Form.Parameters_To_Index_Page (
					User_Id => User_Id,
					User_Password => User_Password));
		elsif Tag = "href_logindex" then
			Forms.Write_Attribute_Name (Output, "href");
			Forms.Write_Link (
				Output,
				Form,
				Current_Directory => ".",
				Resource => Ada.Hierarchical_File_Names.Compose (
					Directory => HTML_Directory,
					Relative_Name => "")); -- add a trailing path delimiter
		elsif Tag = "user" then
			for I in User_List.Iterate loop
				declare
					procedure Handle_User (
						Output : not null access Ada.Streams.Root_Stream_Type'Class;
						Tag : in String;
						Template : in Web.Producers.Template) is
					begin
						if Tag = "id" then
							Forms.Write_In_HTML (
								Output,
								Form,
								Key (I));
						elsif Tag = "joinedcount" then
							Forms.Write_In_HTML (
								Output,
								Form,
								Image (
									Tabula.Villages.Lists.Count_Joined_By (
										Summaries,
										Key (I),
										Filter => (
											Tabula.Villages.Prologue | Tabula.Villages.Playing => False,
											Tabula.Villages.Epilogue | Tabula.Villages.Closed => True),
										Including_Escaped => False)));
						elsif Tag = "renamed" then
							Forms.Write_In_HTML (
								Output,
								Form,
								User_List.Constant_Reference (I).
									Renamed.Constant_Reference);
						else
							Handle (Output, Tag, Template);
						end if;
					end Handle_User;
				begin
					if Tabula.Villages.Lists.Count_Joined_By (
						Summaries,
						Key (I),
						Filter => (
							Tabula.Villages.Prologue | Tabula.Villages.Playing => False,
							Tabula.Villages.Epilogue | Tabula.Villages.Closed => True),
						Including_Escaped => True) > 0
					then
						Web.Producers.Produce (Output, Template, Handler => Handle_User'Access);
					end if;
				end;
			end loop;
		else
			raise Program_Error with "Invalid template """ & Tag & """";
		end if;
	end Handle;
begin
	Web.Producers.Produce (Output, Read (Template), Handler => Handle'Access);
end Vampire.R3.User_List_Page;
