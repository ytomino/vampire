-- The Village of Vampire by YT, このソースコードはNYSLです
package body Tabula.Casts.Cast_IO is
	
	procedure IO_Partial (Serializer : not null access Serialization.Serializer; Item : in out Person'Class) is
		use Serialization;
		use Person_Sex_IO;
	begin
		IO (Serializer, "name", Item.Name);
		IO (Serializer, "work", Item.Work);
		IO (Serializer, "image", Item.Image);
		IO (Serializer, "sex", Item.Sex);
		IO (Serializer, "group", Item.Group);
	end IO_Partial;
	
	use Groups;
	package Groups_IO is new Serialization.IO_List (
		Cursor => Groups.Cursor,
		Element_Type => Group,
		Container_Type => Groups.Vector,
		Default => Empty_Group);
	
	use People;
	package People_IO is new Serialization.IO_List (
		Cursor => People.Cursor,
		Element_Type => Person,
		Container_Type => People.Vector,
		Default => Empty_Person);
	
	use Works;
	package Works_IO is new Serialization.IO_List (
		Cursor => Works.Cursor,
		Element_Type => Work,
		Container_Type => Works.Vector,
		Default => Empty_Work);
	
	procedure IO (Serializer: not null access Serialization.Serializer; Item : in out Cast_Collection) is
		use Serialization;
		use Neutralable_Sex_IO;
		use Groups_IO;
		use People_IO;
		use Works_IO;
		procedure Root_Callback is
			procedure Groups_Callback (Serializer: not null access Serialization.Serializer; Item : in out Group) is
				procedure Group_Callback is
				begin
					IO (Serializer, "name", Item.Name);
					IO (Serializer, "by", Item.By);
					IO (Serializer, "width", Item.Width);
					IO (Serializer, "height", Item.Height);
					IO (Serializer, "group", Item.Group);
				end Group_Callback;
			begin
				IO (Serializer, Group_Callback'Access);
			end Groups_Callback;
			procedure People_Callback (Serializer: not null access Serialization.Serializer; Item : in out Person) is
				procedure Person_Callback is
				begin
					IO_Partial (Serializer, Item);
				end Person_Callback;
			begin
				IO (Serializer, Person_Callback'Access);
			end People_Callback;
			procedure Works_Callback (Serializer: not null access Serialization.Serializer; Item : in out Work) is
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
			IO (Serializer, "groups", Item.Groups, Groups_Callback'Access);
			IO (Serializer, "people", Item.People, People_Callback'Access);
			IO (Serializer, "works", Item.Works, Works_Callback'Access);
		end Root_Callback;
	begin
		IO (Serializer, Root_Callback'Access);
	end IO;
	
end Tabula.Casts.Cast_IO;
