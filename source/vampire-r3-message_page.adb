-- The Village of Vampire by YT, このソースコードはNYSLです
procedure Vampire.R3.Message_Page (
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Form : in Forms.Root_Form_Type'Class;
	Template : in String;
	Base_Page : in Forms.Base_Page;
	Village_Id : in Tabula.Villages.Village_Id := Tabula.Villages.Invalid_Village_Id;
	Village : access constant Tabula.Villages.Village_Type'Class := null;
	Message : in String;
	User_Id : in String;
	User_Password : in String)
is
	procedure Handle (
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Tag : in String;
		Contents : in Web.Producers.Template) is
	begin
		if Tag = "action_uri" then
			String'Write (Output, "action=");
			Forms.Write_Link_To_Resource (
				Output,
				Form,
				Current_Directory => ".",
				Resource => Forms.Self);
		elsif Tag = "message" then
			Forms.Write_In_HTML (Output, Form, Message);
		elsif Tag = "parameters" then
			case Base_Page is
				when Forms.Index_Page =>
					Web.Write_Query_In_HTML (
						Output,
						Form.HTML_Version,
						Form.Parameters_To_Index_Page (
							User_Id => User_Id,
							User_Password => User_Password));
				when Forms.User_Page =>
					Web.Write_Query_In_HTML (
						Output,
						Form.HTML_Version,
						Form.Parameters_To_User_Page (
							User_Id => User_Id,
							User_Password => User_Password));
				when Forms.User_List_Page =>
					raise Program_Error with "unimplemented";
				when Forms.Village_Page =>
					Web.Write_Query_In_HTML (
						Output,
						Form.HTML_Version,
						Form.Parameters_To_Village_Page (
							Village_Id => Village_Id,
							User_Id => User_Id,
							User_Password => User_Password));
			end case;
		elsif Tag = "title" then
			if Village /= null then
				declare
					State : Tabula.Villages.Village_State;
					Today : Natural;
				begin
					Tabula.Villages.Get_State (Village.all, State, Today);
					Forms.Write_In_HTML (
						Output,
						Form,
						Village.Name.Constant_Reference.Element.all & " " & 
							Day_Name (Today, Today, State) & " - ");
				end;
			end if;
		else
			raise Program_Error with "Invalid template """ & Tag & """";
		end if;
	end Handle;
begin
	Produce (Output, Template, Handler => Handle'Access);
end Vampire.R3.Message_Page;
