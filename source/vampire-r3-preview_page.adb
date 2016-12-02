-- The Village of Vampire by YT, このソースコードはNYSLです
procedure Vampire.R3.Preview_Page (
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Form : in Forms.Root_Form_Type'Class;
	Template : in String;
	Image_Directory : in String;
	Village_Id : in Tabula.Villages.Village_Id;
	Village : in Villages.Village_Type; 
	Message : in Villages.Message;
	User_Id : in String;
	User_Password : in String)
is
	procedure Handle (
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Tag : in String;
		Template : in Web.Producers.Template) is
	begin
		if Tag = "action_page" then
			Forms.Write_Attribute_Name (Output, "action");
			Forms.Write_Link (
				Output,
				Form,
				Current_Directory => ".",
				Resource => Forms.Self,
				Parameters => Form.Parameters_To_Village_Page (
					Village_Id => Village_Id,
					User_Id => User_Id,
					User_Password => User_Password));
		elsif Tag = "villagename" then
			Forms.Write_In_HTML (
				Output,
				Form,
				Village.Name.Constant_Reference);
		elsif Tag = "speech" then
			Handle_Speech (
				Output,
				Template,
				Form => Form,
				Current_Directory => ".",
				Image_Directory => Image_Directory,
				Face_Width => Village.Face_Width,
				Face_Height => Village.Face_Height,
				Subject => Village.People.Constant_Reference (Message.Subject),
				Text => Message.Text.Constant_Reference,
				Time => Message.Time,
				Filter => "");
		elsif Tag = "value_kind" then
			Forms.Write_Attribute_Name (Output, "value");
			Forms.Write_Attribute_Open (Output);
			Forms.Write_In_Attribute (Output, Form, Villages.Message_Kind'Image (Message.Kind));
			Forms.Write_Attribute_Close (Output);
		elsif Tag = "value_text" then
			Forms.Write_Attribute_Name (Output, "value");
			Forms.Write_Attribute_Open (Output);
			Forms.Write_In_Attribute (Output, Form, Message.Text.Constant_Reference);
			Forms.Write_Attribute_Close (Output);
		elsif Tag = "longer" then
			if Message.Text.Length > Villages.Max_Length_Of_Message then
				Web.Producers.Produce (Output, Template, Handler => Handle'Access);
			end if;
		elsif Tag = "ok" then
			if Message.Text.Length <= Villages.Max_Length_Of_Message then
				Web.Producers.Produce (Output, Template, Handler => Handle'Access);
			end if;
		else
			Raise_Unknown_Tag (Tag);
		end if;
	end Handle;
begin
	Web.Producers.Produce (Output, Read (Template), Handler => Handle'Access);
end Vampire.R3.Preview_Page;
