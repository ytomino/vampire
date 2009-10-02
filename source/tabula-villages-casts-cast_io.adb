-- The Village of Vampire by YT, このソースコードはNYSLです
with Tabula.Villages.Village_IO;
package body Tabula.Villages.Casts.Cast_IO is
	use Works;

	package Works_IO is new DYAYaml.IO_List(Works.Vector, Works.Cursor, Work, Default_Work);
	
	procedure IO(Serializer: in out DYAYaml.Serializer; Cast: in out Cast_Type) is
		use DYAYaml;
		use Tabula.Villages.Village_IO;
		use Tabula.Villages.Village_IO.Sex_Kind_IO;
		use Works_IO;
		procedure Root_Callback is
			procedure Works_Callback(Item : in out Work) is
				procedure Work_Callback is
				begin
					IO(Serializer, "name", Item.Name);
					IO(Serializer, "sex", Item.Sex);
					IO(Serializer, "nominated", Item.Nominated);
				end Work_Callback;
			begin
				IO(Serializer, Work_Callback'Access);
			end Works_Callback;
		begin
			IO(Serializer, "people", Cast.People);
			IO(Serializer, "works", Cast.Works, Works_Callback'Access);
		end Root_Callback;
	begin
		IO(Serializer, Root_Callback'Access);
	end IO;
	
end Tabula.Villages.Casts.Cast_IO;
