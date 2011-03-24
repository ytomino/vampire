-- The Village of Vampire by YT, このソースコードはNYSLです
procedure Vampire.R3.Register_Page (
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Form : in Forms.Root_Form_Type'Class;
	Template : in String;
	Base_Page : in Forms.Base_Page;
	Village_Id : in Tabula.Villages.Village_Id := Tabula.Villages.Invalid_Village_Id;
	New_User_Id : in String;
	New_User_Password : in String)
is
	procedure Handle (
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Tag : in String;
		Template : in Web.Producers.Template) is
	begin
		if Tag = "action_cgi" then
			String'Write (Output, "action=");
			Forms.Write_Link (
				Output,
				Form,
				Current_Directory => ".",
				Resource => Forms.Self);
		elsif Tag = "action_page" then
			String'Write (Output, "action=");
			Forms.Write_Link (
				Output,
				Form,
				Current_Directory => ".",
				Resource => Forms.Self,
				Parameters => Form.Parameters_To_Base_Page (
					Base_Page => Base_Page,
					Village_Id => Village_Id,
					User_Id => "",
					User_Password => ""));
		elsif Tag = "id" then
			Forms.Write_In_HTML (Output, Form, New_User_Id);
		elsif Tag = "parameters" then
			Web.Write_Query_In_HTML (
				Output,
				Form.HTML_Version,
				Form.Parameters_To_Base_Page (
					Base_Page => Base_Page,
					Village_Id => Village_Id,
					User_Id => "",
					User_Password => ""));
		elsif Tag = "value_newid" then
			String'Write (Output, "value=""");
			Forms.Write_In_Attribute (Output, Form, New_User_Id);
			Character'Write (Output, '"');
		elsif Tag = "value_newpassword" then
			String'Write (Output, "value=""");
			Forms.Write_In_Attribute (Output, Form, New_User_Password);
			Character'Write (Output, '"');
		else
			raise Program_Error with "Invalid template """ & Tag & """";
		end if;
	end Handle;
begin
	Produce (Output, Template, Handler => Handle'Access);
end Vampire.R3.Register_Page;
