-- The Village of Vampire by YT, このソースコードはNYSLです
package Vampire.Renderers.Simple is
	
	type Renderer is new Renderers.Renderer with null record;
	
	-- Page Generating
	
private
	
	overriding procedure Produce(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		File_Name : in String;
		Handler : not null access procedure(Output : not null access Ada.Streams.Root_Stream_Type'Class; Tag : in String; Contents : Web.Producers.Template));
	
	overriding procedure Link(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Village_Id : Tabula.Villages.Village_Id := Tabula.Villages.Invalid_Village_Id;
		Day : Integer := -1;
		First : Integer := -1;
		Last : Integer := -1;
		Latest : Integer := -1;
		Log : Boolean := False;
		User_Id : in String;
		User_Password : in String;
		User_Page: Boolean := False);
	
	overriding function HTML_Version(Object : in Renderer) return Web.HTML_Version;

end Vampire.Renderers.Simple;
