import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Scanner;

class Part1 {
   public static void main(String[] args) throws IOException {
      final var path = "./input.txt";
      var dims = readDims(path);
      System.out.println(dims);

      long total = 0;
      for (var dim : dims) {
         total += dim.wrappingPaper();
      }

      System.out.println(total);
   }

   public static ArrayList<Dimensions> readDims(String path) throws IOException {
      var lines = Files.readAllLines(Paths.get(path), StandardCharsets.UTF_8);
      var output = new ArrayList<Dimensions>();

      for (String line : lines) {
         var dim = Dimensions.read(line);
         output.add(dim);
      }

      return output;
   }
}

public record Dimensions(long l, long w, long h) {
   public long surfaceArea() {
      return 2 * (this.l*this.w + this.l*this.h + this.w*this.h);
   }

   public long max() {
      return Math.max(this.l, Math.max(this.w, this.h));
   }

   public long wrappingPaper() {
      long additional = this.l * this.w * this.h / this.max();
      return this.surfaceArea() + additional;
   }

   public static Dimensions read(String line) {
      final String[] elems = line.split("x");
      long l = Long.parseLong(elems[0]);
      long w = Long.parseLong(elems[1]);
      long h = Long.parseLong(elems[2]);
      return new Dimensions(l, w, h);
   }
}
