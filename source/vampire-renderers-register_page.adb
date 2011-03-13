-- The Village of Vampire by YT, このソースコードはNYSLです
procedure Tabula.Renderers.Register_Page (
	Object : in Renderer'Class;
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Village_Id : in Villages.Village_Id := Villages.Invalid_Village_Id;
	New_User_Id : in String;
	New_User_Password : in String)
is
	procedure Handle(Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Tag : in String; Template : in Web.Producers.Template) is
	begin
		if Tag = "id" then
			Write(Output, New_User_Id);
		elsif Tag = "newid" then
			Write(Output, '"');
			Write(Output, New_User_Id);
			Write(Output, '"');
		elsif Tag = "newpassword" then
			Write(Output, '"');
			Web.Write_In_Attribute (Output, Object.HTML_Version, New_User_Password);
			Write(Output, '"');
		else
			Handle_Users(Output, Tag, Template, Object,
				Village_Id => Village_Id, User_Id => "", User_Password => "");
		end if;
	end Handle;
begin
	Produce (Object, Output, Object.Configuration.Template_Register_File_Name.all, Handle'Access);
end Tabula.Renderers.Register_Page;
