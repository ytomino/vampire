-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Strings.Unbounded;
procedure Vampire.Renderers.Message_Page(
	Object : in Renderer'Class;
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Village_Id : in Tabula.Villages.Village_Id := Tabula.Villages.Invalid_Village_Id;
	Village : access Vampire.Villages.Village_Type := null;
	Message : in String;
	User_Id : in String;
	User_Password : in String) 
is
	use Tabula.Villages;
	procedure Handle(Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Tag : in String; Contents : in Web.Producers.Template) is
	begin
		if Tag = "message" then
			Web.Write_In_HTML (Output, Object.HTML_Version, Message);
		elsif Tag = "uri" then
			Link(Object, Output, Village_Id => Village_Id,
				User_Id => User_Id, User_Password => User_Password);
		elsif Tag = "villageid" then
			if Village_Id = Invalid_Village_Id then
				Write(Output, """""");
			else
				Write(Output, '"');
				Write(Output, Village_Id);
				Write(Output, '"');
			end if;
		elsif Tag = "title" then
			if Village /= null then
				Web.Write_In_HTML (Output, Object.HTML_Version, Ada.Strings.Unbounded.To_String(Village.Name));
				Write(Output, ' ');
				Day_Name(Object, Output, Village.Today, Village.Today, Village.State);
				Write(Output, " - ");
			end if;
		elsif Tag = "invillage" then
			if Village /= null or else Village_Id /= Invalid_Village_Id then
				Web.Producers.Produce(Output, Contents, Handler => Handle'Access);
			end if;
		elsif Tag = "id" then
			Write(Output, '"');
			Web.Write_In_Attribute (Output, Object.HTML_Version, User_Id);
			Write(Output, '"');
		elsif Tag = "password" then
			Write(Output, '"');
			Web.Write_In_Attribute (Output, Object.HTML_Version, User_Password);
			Write(Output, '"');
		else
			raise Program_Error with "Invalid template """ & Tag & """";
		end if;
	end Handle;
begin
	Produce(Object, Output, Object.Configuration.Template_Message_File_Name.all, Handle'Access);
end Vampire.Renderers.Message_Page;
