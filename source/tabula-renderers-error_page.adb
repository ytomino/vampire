procedure Tabula.Renderers.Error_Page(
	Object : in Renderer'Class;
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Message : in String) 
is
	procedure Handle(Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Tag : in String; Template : in Web.Producers.Template) is
	begin
		if Tag = "message" then
			Write(Output, Web.Markup_Entity(Message));
		elsif Tag = "uri" then
			Link(Object, Output, Village_Id => Villages.Lists.Invalid_Village_Id,
				User_Id => "", User_Password => "");
		else
			raise Program_Error with "Invalid template """ & Tag & """";
		end if;
	end Handle;
begin
	Produce(Object, Output, Object.Configuration.Template_Error_File_Name.all, Handle'Access);
end Tabula.Renderers.Error_Page;
