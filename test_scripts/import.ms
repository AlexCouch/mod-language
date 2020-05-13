import LibraryTest;
import LibraryTest2 as L2;

fn test -> bool {
  LibraryTest::C::D::E();
  LibraryTest::X == L2::X
}