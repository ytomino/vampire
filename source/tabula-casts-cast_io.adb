-- The Village of Vampire by YT, このソースコードはNYSLです
package body Tabula.Casts.Cast_IO is
	
	procedure IO_Partial (
		Serializer : not null access Serialization.Serializer;
		Item : in out Person'Class)
	is
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
	package Groups_IO is
		new Serialization.IO_List (
			Cursor => Groups.Cursor,
			Element_Type => Group,
			Container_Type => Groups.Vector,
			Reference_Type => Groups.Reference_Type,
			Default => Empty_Group,
			Next => Groups.Cursor'Succ);
	
	use People;
	package People_IO is
		new Serialization.IO_List (
			Cursor => People.Cursor,
			Element_Type => Person,
			Container_Type => People.Vector,
			Reference_Type => People.Reference_Type,
			Default => Empty_Person,
			Has_Element => People.Has_Element,
			Next => People.Cursor'Succ);
	
	use Works;
	package Works_IO is
		new Serialization.IO_List (
			Cursor => Works.Cursor,
			Element_Type => Work,
			Container_Type => Works.Vector,
			Reference_Type => Works.Reference_Type,
			Default => Empty_Work,
			Has_Element => Works.Has_Element,
			Next => Works.Cursor'Succ);
	
	procedure IO (
		Serializer : not null access Serialization.Serializer;
		Item : in out Cast_Collection)
	is
		use Serialization;
		use Neutralable_Sex_IO;
		use Groups_IO;
		use People_IO;
		use Works_IO;
		procedure Groups_Callback (
			Serializer : not null access Serialization.Serializer;
			Item : in out Group) is
		begin
			for P in IO (Serializer) loop
				IO (Serializer, "name", Item.Name);
				IO (Serializer, "by", Item.By);
				IO (Serializer, "width", Item.Width);
				IO (Serializer, "height", Item.Height);
				IO (Serializer, "group", Item.Group);
			end loop;
		end Groups_Callback;
		procedure People_Callback (
			Serializer : not null access Serialization.Serializer;
			Item : in out Person) is
		begin
			for P in IO (Serializer) loop
				IO_Partial (Serializer, Item);
			end loop;
		end People_Callback;
		procedure Works_Callback (
			Serializer : not null access Serialization.Serializer;
			Item : in out Work) is
		begin
			for P in IO (Serializer) loop
				IO (Serializer, "name", Item.Name);
				IO (Serializer, "sex", Item.Sex);
				IO (Serializer, "nominated", Item.Nominated);
			end loop;
		end Works_Callback;
	begin
		for P in IO (Serializer) loop
			IO (Serializer, "groups", Item.Groups, Groups_Callback'Access);
			IO (Serializer, "people", Item.People, People_Callback'Access);
			IO (Serializer, "works", Item.Works, Works_Callback'Access);
		end loop;
	end IO;
	
end Tabula.Casts.Cast_IO;
