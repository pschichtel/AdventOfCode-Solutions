import java.security.MessageDigest

println("Day  4")

val input = "iwrupvqb"

lazy val hasher = MessageDigest.getInstance("MD5")

def md5(s: String) = hasher.digest(s.getBytes).map("%02X".format(_)).mkString

def startsWith(prefix: String): PartialFunction[(Int, String), Boolean] = {
    {
        case (n, s: String) => s.startsWith(prefix)
    }
}

def hashStream(input: String, number: Int = 0): Stream[(Int, String)] = (number, md5(input + number)) #:: hashStream(input, number + 1)

var (n1, _) = hashStream(input).filter(startsWith("00000")).head
println(s"Part 1: $n1")

val (n2, _) = hashStream(input).filter(startsWith("000000")).head
println(s"Part 2: $n2")

