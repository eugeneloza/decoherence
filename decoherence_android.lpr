{$mode objfpc}{$H+}
library Decoherence_Android;
uses CastleAndroidNativeAppGlue, Decoherence, CastleMessaging, global_var;
exports
  Java_net_sourceforge_castleengine_MainActivity_jniMessage,
  ANativeActivity_onCreate;
end.
