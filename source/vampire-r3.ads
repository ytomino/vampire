-- The Village of Vampire by YT, このソースコードはNYSLです
private with Ada.Streams;
private with Web.Producers;
package Vampire.R3 is
	
private
	
	procedure Produce (
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Template_Source : in String;
		Template_Cache : in String := "";
		Handler : not null access procedure (
			Output : not null access Ada.Streams.Root_Stream_Type'Class;
			Tag : in String;
			Contents : Web.Producers.Template));
	
end Vampire.R3;
