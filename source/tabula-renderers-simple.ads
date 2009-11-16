-- The Village of Vampire by YT, このソースコードはNYSLです
package Tabula.Renderers.Simple is
	
	type Renderer is new Renderers.Renderer with null record;
	
	-- Info
	
	overriding function Get_Village_Id(
		Object : Renderer; 
		Query_Strings : Web.Query_Strings) return Villages.Village_Id;
	
	overriding procedure Get_Day(
		Object : in Renderer; 
		Village : in Vampires.Villages.Village_Type; 
		Query_Strings : in Web.Query_Strings; 
		Day : out Natural);
	
	overriding procedure Get_Range(
		Object : in Renderer; 
		Village : in Vampires.Villages.Village_Type; 
		Day : in Natural;
		Query_Strings : in Web.Query_Strings; 
		First, Last : out Integer);
	
	overriding function Get_User_Id(
		Object : Renderer; 
		Query_Strings : Web.Query_Strings;
		Cookie : Web.Cookie) return String;
	
	overriding function Get_User_Password(
		Object : Renderer; 
		Query_Strings : Web.Query_Strings;
		Cookie : Web.Cookie) return String;
	
	overriding procedure Set_User(
		Object : in Renderer; 
		Cookie : in out Web.Cookie;
		User_Id: in String;
		User_Password : in String);
	
	overriding function Get_Text(
		Object : Renderer; 
		Inputs : Web.Query_Strings) return String;
	
	overriding function Is_User_Page(
		Object : Renderer; 
		Query_Strings : Web.Query_Strings;
		Cookie : Web.Cookie) return Boolean;
	
	-- Page Generating
	
	overriding procedure Refresh_Page(
		Object : in Renderer; 
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		URI : in String);
	
	overriding procedure Village_Page(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Village_Id : Villages.Village_Id; 
		Village : Vampires.Villages.Village_Type; 
		Day : Natural;
		First, Last : Integer := -1;
		User_Id : String;
		User_Password : String);
	
private
	
	overriding procedure Produce(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		File_Name : in String;
		Handler : not null access procedure(Output : not null access Ada.Streams.Root_Stream_Type'Class; Tag : in String; Contents : Web.Producers.Template));
	
	overriding procedure Link(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Village_Id : Villages.Village_Id := Villages.Invalid_Village_Id;
		Day : Integer := -1;
		First : Integer := -1;
		Last : Integer := -1;
		Latest : Integer := -1;
		Log : Boolean := False;
		User_Id : in String;
		User_Password : in String;
		User_Page: Boolean := False);
	
	overriding function HTML_Version(Object : in Renderer) return Web.HTML_Version;

end Tabula.Renderers.Simple;
