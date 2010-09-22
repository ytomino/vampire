-- The Village of Vampire by YT, このソースコードはNYSLです
with Tabula.Calendar.Time_IO;
package body Tabula.Users.User_Info_IO is
	
	procedure IO (Serializer: not null access Serialization.Serializer; Value: in out User_Info) is
		use Serialization;
		use Tabula.Calendar.Time_IO;
		procedure Callback is
		begin
			IO (Serializer, "password", Value.Password);
			IO (Serializer, "remote-addr", Value.Remote_Addr);
			IO (Serializer, "remote-host", Value.Remote_Host);
			IO (Serializer, "creation-time", Value.Creation_Time);
			IO (Serializer, "last-remote-addr", Value.Last_Remote_Addr);
			IO (Serializer, "last-remote-host", Value.Last_Remote_Host);
			IO (Serializer, "last-time", Value.Last_Time);
			IO (Serializer, "ignore-request", Value.Ignore_Request, Default => False);
			IO (Serializer, "disallow-new-village", Value.Disallow_New_Village, Default => False);
			IO (Serializer, "no-log", Value.No_Log, Default => False);
			IO (Serializer, "renamed", Value.Renamed, Default => Ada.Strings.Unbounded.Null_Unbounded_String);
		end Callback;
	begin
		IO (Serializer, Callback'Access);
	end IO;
	
end Tabula.Users.User_Info_IO;
