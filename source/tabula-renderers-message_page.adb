with Ada.Strings.Unbounded;
procedure Tabula.Renderers.Message_Page(
	Object : in Renderer'Class;
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Village_Id : in Villages.Lists.Village_Id := Villages.Lists.Invalid_Village_Id;
	Village : access Villages.Village_Type := null;
	Message : in String;
	User_Id : in String;
	User_Password : in String) 
is
	procedure Handle(Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Tag : in String; Contents : in Web.Producers.Template) is
	begin
		if Tag = "message" then
			Write(Output, Web.Markup_Entity(Message));
		elsif Tag = "uri" then
			Link(Object, Output, Village_Id => Village_Id,
				User_Id => User_Id, User_Password => User_Password);
		elsif Tag = "villageid" then
			if Village_Id = Villages.Lists.Invalid_Village_Id then
				Write(Output, """""");
			else
				Write(Output, '"');
				Write(Output, Village_Id);
				Write(Output, '"');
			end if;
		elsif Tag = "title" then
			if Village /= null then
				Write(Output, Web.Markup_Entity(Ada.Strings.Unbounded.To_String(Village.Name)));
				Write(Output, ' ');
				Day_Name(Object, Output, Village.Today, Village.Today, Village.State);
				Write(Output, " - ");
			end if;
		elsif Tag = "invillage" then
			Web.Producers.Produce(Output, Contents, Handler => Handle'Access);
		elsif Tag = "id" then
			Write(Output, '"');
			Write(Output, User_Id);
			Write(Output, '"');
		elsif Tag = "password" then
			Write(Output, '"');
			Write(Output, Web.Markup_Entity(User_Password));
			Write(Output, '"');
		else
			raise Program_Error with "Invalid template """ & Tag & """";
		end if;
	end Handle;
begin
	Produce(Object, Output, Object.Configuration.Template_Message_File_Name.all, Handle'Access);
end Tabula.Renderers.Message_Page;
