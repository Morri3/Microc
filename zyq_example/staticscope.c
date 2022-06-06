//静态作用域
int res;
int b() {
  print res;
}
int a() {
  int res=5;
  return b();
}
void main(){
  res=3;
  a();
}