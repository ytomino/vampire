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
		if Tag = "action_cgi" then
			Forms.Write_Attribute_Name (Output, "action");
			Forms.Write_Link (
				Output,
				Form,
				Current_Directory => ".",
				Resource => Forms.Self);
		elsif Tag = "message" then
			Forms.Write_In_HTML (Output, Form, Message);
		elsif Tag = "parameters" then
			Web.Write_Query_In_HTML (
				Output,
				Form.HTML_Version,
				Form.Parameters_To_Base_Page (
					Base_Page => Base_Page,
					Village_Id => Village_Id,
					User_Id => User_Id,
					User_Password => User_Password));
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
	Web.Producers.Produce (Output, Read (Template), Handler => Handle'Access);
end Vampire.R3.Message_Page;
