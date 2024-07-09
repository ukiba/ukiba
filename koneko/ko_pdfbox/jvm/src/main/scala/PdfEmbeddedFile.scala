package jp.ukiba.koneko
package ko_pdfbox

import jp.ukiba.koinu.ko_java.io.CountingInputStream

import org.apache.pdfbox.io.RandomAccessReadBuffer
import org.apache.pdfbox.pdmodel.{PDDocument, PDDocumentNameDictionary, PageMode}
import org.apache.pdfbox.pdmodel.{PDEmbeddedFilesNameTreeNode}
import org.apache.pdfbox.pdmodel.common.PDNameTreeNode
import org.apache.pdfbox.pdmodel.common.filespecification.{PDComplexFileSpecification, PDEmbeddedFile}
import org.apache.pdfbox.pdmodel.interactive.annotation.PDAnnotationFileAttachment
import org.apache.pdfbox.pdmodel.encryption.{StandardProtectionPolicy, AccessPermission}
import org.apache.pdfbox.cos.COSName
import org.apache.pdfbox.Loader
import org.apache.commons.logging.LogFactory // pdfbox depends on commons logging

import scala.util.Using
import scala.jdk.CollectionConverters._
import java.io.{OutputStream, InputStream}
import java.time.{ZonedDateTime, ZoneId}
import java.util.{GregorianCalendar, Calendar}
import java.nio.file.{Path, Files}

/** Attaches / detaches files */
class PdfEmbeddedFile:
  import PdfEmbeddedFile.*

  val log = LogFactory.getLog(classOf[PdfEmbeddedFile])

  val timeZone = ZoneId.systemDefault
  def toZonedDateTime(cal: Calendar) = cal match
    case gcal: GregorianCalendar => gcal.toZonedDateTime
    case cal => cal.toInstant.atZone(timeZone)

  def embed(doc: PDDocument, embeddedFiles: EmbeddedFile*): Unit =
    // based on https://github.com/apache/pdfbox/blob/2.0.27/examples/src/main/java/org/apache/pdfbox/examples/pdmodel/EmbeddedFiles.java

    val namesMap = embeddedFiles.zipWithIndex.map: (embeddedFile, index) =>
      // create the file specification, which holds the embedded file
      val fileSpec = PDComplexFileSpecification()

      // use both methods for backwards, cross-platform and cross-language compatibility.
      fileSpec.setFile(embeddedFile.name)
      fileSpec.setFileUnicode(embeddedFile.name)

      // the PDStream constructor copies the entire InputStream
      // https://github.com/apache/pdfbox/blob/3.0.3/pdfbox/src/main/java/org/apache/pdfbox/pdmodel/common/PDStream.java#L137
      val cin = CountingInputStream(embeddedFile.in)
      val pdStream = PDEmbeddedFile(doc, cin, COSName.FLATE_DECODE) // always compress

      // optional parameters
      if (cin.eof)
        for (size <- cin.intCount)
          pdStream.setSize(size)

      for (subtype <- embeddedFile.subtype)
        pdStream.setSubtype(subtype)
      for (creationDate <- embeddedFile.creationDateCalendar)
        pdStream.setCreationDate(creationDate)
      for (modDate <- embeddedFile.modDateCalendar)
        pdStream.setModDate(modDate)

      // use both methods for backwards, cross-platform and cross-language compatibility
      fileSpec.setEmbeddedFile(pdStream)
      fileSpec.setEmbeddedFileUnicode(pdStream)
      for (desc <- embeddedFile.desc)
        fileSpec.setFileDescription(desc)

      s"attachment ${index + 1}" -> fileSpec
    .toMap

    // create a new tree node and add the embedded file
    val treeNode = PDEmbeddedFilesNameTreeNode()
    treeNode.setNames(namesMap.asJava)
    val kids = List(treeNode) // add the new node as kid to the root node

    // embedded files are stored in a named tree
    val efTree = PDEmbeddedFilesNameTreeNode()
    efTree.setKids(kids.asJava)

    // add the tree to the document catalog
    val names = PDDocumentNameDictionary(doc.getDocumentCatalog)
    names.setEmbeddedFiles(efTree)
    doc.getDocumentCatalog.setNames(names)

    // show attachments panel in some viewers
    doc.getDocumentCatalog.setPageMode(PageMode.USE_ATTACHMENTS)

  def embed(outFile: Path, inFile: Path, password: Option[String], embeddedFiles: EmbeddedFile*): Unit =
    Using.Manager: use =>
      val doc = use(Loader.loadPDF(inFile.toFile, password.getOrElse("")))
      val out = use(Files.newOutputStream(outFile))
      embed(doc, embeddedFiles*)

      // avoid `PDF contains an encryption dictionary, please remove it with setAllSecurityToBeRemoved() or set a protection policy with protect()`
      if doc.isEncrypted then
        doc.protect(StandardProtectionPolicy(password.mkString,
            "", // TODO userPassword
            AccessPermission(doc.getEncryption.getPermissions)))

      doc.save(out)
    .get // throws exception

  def embed(out: OutputStream, in: InputStream, password: Option[String], embeddedFiles: EmbeddedFile*): Unit = {
    Using.Manager: use =>
      val inRead = use(RandomAccessReadBuffer(in))
      val doc    = use(Loader.loadPDF(inRead, password.getOrElse("")))
      embed(doc, embeddedFiles*)

      // avoid `PDF contains an encryption dictionary, please remove it with setAllSecurityToBeRemoved() or set a protection policy with protect()`
      if doc.isEncrypted then
        doc.protect(StandardProtectionPolicy(password.mkString,
            "", // TODO userPassword
            AccessPermission(doc.getEncryption.getPermissions)))

      doc.save(out)
    .get // throws exception
  }

  def extract(doc: PDDocument, callback: EmbeddedFile => Unit): Unit =
    // based on https://github.com/apache/pdfbox/blob/2.0.27/examples/src/main/java/org/apache/pdfbox/examples/pdmodel/EmbeddedFiles.java

    def extractFromTreeNode(efTree: PDNameTreeNode[PDComplexFileSpecification]): Unit =
      Option(efTree.getNames) match
        case Some(namesMap) => namesMap.asScala.foreach(entry => extractFile(entry._2, callback))
        case None => Option(efTree.getKids) match
          case Some(kids) => kids.asScala.foreach(extractFromTreeNode) // recursive
          case None =>
    val names = PDDocumentNameDictionary(doc.getDocumentCatalog)
    Option(names.getEmbeddedFiles) match
      case Some(efTree) => extractFromTreeNode(efTree)
      case None =>

    // extract files from page annotations
    for (page <- doc.getPages.asScala)
      for (anno <- page.getAnnotations.asScala)
        anno match
          case anno: PDAnnotationFileAttachment =>
            anno.getFile match
              case fileSpec: PDComplexFileSpecification => extractFile(fileSpec, callback)
              case file => log.warn(s"PDAnnotationFileAttachment.getFile: Unexpected type: ${
                  Option(file).map(_.getClass.getName).orNull}")
          case _ =>

  def extractFile(fileSpec: PDComplexFileSpecification, callback: EmbeddedFile => Unit): Unit =
    Option(fileSpec.getEmbeddedFileUnicode) orElse
        Option(fileSpec.getEmbeddedFileDos) orElse
        Option(fileSpec.getEmbeddedFileMac) orElse
        Option(fileSpec.getEmbeddedFileUnix) orElse
        Option(fileSpec.getEmbeddedFile) match
      case Some(pdStream) => // PDEmbeddedFile
        Using(pdStream.createInputStream): in =>
          callback(EmbeddedFile(fileSpec.getFilename,
              in,
              Option(pdStream.getSubtype),
              Option(pdStream.getCreationDate).map(toZonedDateTime),
              Option(pdStream.getModDate).map(toZonedDateTime),
              Option(fileSpec.getFileDescription)))
        .get

      case None =>

  def extract(inFile: Path, password: Option[String], callback: EmbeddedFile => Unit): Unit =
    Using(Loader.loadPDF(inFile.toFile, password.getOrElse(""))): doc =>
      extract(doc, callback)
    .get // throws exception

  def extract(in: InputStream, password: Option[String], callback: EmbeddedFile => Unit): Unit =
    Using.Manager: use =>
      val inRead = use(RandomAccessReadBuffer(in))
      val doc    = use(Loader.loadPDF(inRead, password.getOrElse("")))
      extract(doc, callback)
    .get // throws exception

object PdfEmbeddedFile:
  case class EmbeddedFile(
    name: String,
    in: InputStream,
    subtype: Option[String] = None, // e.g., text/plain
    creationDate: Option[ZonedDateTime] = None,
    modDate: Option[ZonedDateTime] = None,
    desc: Option[String] = None,
  ):
    def creationDateCalendar: Option[GregorianCalendar] = creationDate.map(GregorianCalendar.from(_))
    def modDateCalendar     : Option[GregorianCalendar] = modDate     .map(GregorianCalendar.from(_))

/*
  def main(args: Array[String]): Unit = {
    args.head match {
      case "embed" =>
        Using.Manager: use =>
          PdfEmbeddedFile().embed(Path.of(args(1)), Path.of(args(2)), None,
              args.drop(3).map(Path.of(_)).map { file =>
                EmbeddedFile(file.getFileName.toString, use(Files.newInputStream(file)))
              }*)
      case _ => throw new IllegalArgumentException(args.mkString(" "))
    }
  }
*/
