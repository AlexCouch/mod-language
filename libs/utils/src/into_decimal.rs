/// Convert a number to a decimal inside given a character buffer
/// 
/// Returns the number of digits that were required
/// 
/// Panics if the buff provided didnt have enough room (up to 20 digits may be required for a u64)
pub fn into_decimal (mut num: u64, buff: &mut [u8]) -> usize {
  let mut work = [0u8; 20];
  let mut count = 0;

  loop {
    work[count] = (num % 10) as _;
    count += 1;
    num /= 10;

    if num == 0 { break }
  }
  
  let mut offset = 0;
  
  while count > 0 {
    buff[offset] = b'0' + work[count - 1];
    offset += 1;
    count -= 1;
  }

  offset
}