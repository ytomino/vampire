-- The Village of Vampire by YT, このソースコードはNYSLです
package body Tabula.Casts.Cast_IO is
	
	procedure IO (Serializer : not null access Serialization.Serializer; Item : in out Person'Class) is
		use Serialization;
		use Person_Sex_IO;
	begin
		IO (Serializer, "name", Item.Name);
		IO (Serializer, "work", Item.Work);
		IO (Serializer, "image", Item.Image);
		IO (Serializer, "sex", Item.Sex);
		IO (Serializer, "group", Item.Group);
	end IO;
	
	use People;
	package People_IO is new Serialization.IO_List (People.Vector, People.Cursor, Person, Default_Person);
	
	use Works;
	package Works_IO is new Serialization.IO_List (Works.Vector, Works.Cursor, Work, Default_Work);
	
	procedure IO (Serializer: not null access Serialization.Serializer; Item : in out Cast_Collection) is
		use Serialization;
		use Sex_Kind_IO;
		use People_IO;
		use Works_IO;
		procedure Root_Callback is
			procedure People_Callback (Item : in out Person) is
				procedure Person_Callback is
				begin
					IO (Serializer, Item);
				end;
			begin
				IO (Serializer, Person_Callback'Access);
			end People_Callback;
			procedure Works_Callback (Item : in out Work) is
				procedure Work_Callback is
				begin
					IO (Serializer, "name", Item.Name);
					IO (Serializer, "sex", Item.Sex);
					IO (Serializer, "nominated", Item.Nominated);
				end Work_Callback;
			begin
				IO (Serializer, Work_Callback'Access);
			end Works_Callback;
		begin
			IO (Serializer, "people", Item.People, People_Callback'Access);
			IO (Serializer, "works", Item.Works, Works_Callback'Access);
		end Root_Callback;
	begin
		IO (Serializer, Root_Callback'Access);
	end IO;
	
end Tabula.Casts.Cast_IO;
