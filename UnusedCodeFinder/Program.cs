using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.FindSymbols;
using Microsoft.CodeAnalysis.MSBuild;

namespace CSharpUnusedTypesAndMembers
{
    // It doesn't work well for extension classes.
    // If the type is eventually resolved through reflection or dependency injection mechanisms.
    // If the type is used in XAML
    // If the type is referenced in interface
    // If the type is interface itself
    // Main method class
    // When searching for references, the whole solution is used.

    internal class Program
    {
        static async Task Main()
        {
            var solutionPath = @"d:\Projects\BM_MAINLINE\ConfigUtil_Original\configurationutility\ConfigurationUtility.sln";
            var projectsToExclude = new string[] { "ConfigurationUtilityTest" };
            var progress = new Progress<string>();
            progress.ProgressChanged += (s, a) => Console.WriteLine(a);
            var unusedTypes = await FindUnusedTypes(solutionPath, projectsToExclude, progress);
            // export unused types
        }

        private static async Task<List<INamedTypeSymbol>> FindUnusedTypes(string solutionFilePath, string[] projectsToExclude, IProgress<string> progress)
        {
            progress.Report("Opening solution...");
            using var workspace = MSBuildWorkspace.Create();
            var solution = await workspace.OpenSolutionAsync(solutionFilePath);
            
            progress.Report("Obtaining types...");
            var projects = solution.Projects.Where(p => !projectsToExclude.Contains(p.Name)).ToList();
            var types = new List<INamedTypeSymbol>();
            foreach (var project in projects)
                types.AddRange(await GetTypes(project));

            progress.Report("Unused types:");
            var unusedTypes = await GetUnusedTypes(types, solution, progress);
            return unusedTypes.ToList();
        }

        private static async Task<IEnumerable<INamedTypeSymbol>> GetUnusedTypes(IEnumerable<INamedTypeSymbol> types, Solution solution, IProgress<string> progress)
        {
            var unusedTypes = new List<INamedTypeSymbol>();

            foreach (var type in types)
            {
                var isUnused = await IsTypeUnused(type, solution);
                if (isUnused)
                {
                    unusedTypes.Add(type);
                    progress.Report(type.ToString() ?? "");
                }
            }

            return unusedTypes;
        }

        private static async Task<bool> IsTypeUnused(INamedTypeSymbol type, Solution solution)
        {
            var references = await SymbolFinder.FindReferencesAsync(type, solution);
            foreach (var reference in references)
            {
                var isReferencedOutsideItsOwnClass = await IsReferencedOutsideItsOwnClass(reference, type);
                if (isReferencedOutsideItsOwnClass)
                    return false;
            }

            return true;
        }

        private static async Task<bool> IsReferencedOutsideItsOwnClass(ReferencedSymbol referencedSymbol, INamedTypeSymbol typeSymbol)
        {
            var locations = referencedSymbol.Locations;

            foreach (var location in locations)
            {
                var document = location.Document;
                var syntaxRoot = await document.GetSyntaxRootAsync();
                var semanticModel = await document.GetSemanticModelAsync();
                if (semanticModel == null)
                    continue;
                var syntaxNode = syntaxRoot?.FindToken(location.Location.SourceSpan.Start).Parent;
                var classDeclarationSyntax = GetParents(syntaxNode).FirstOrDefault(p => p is ClassDeclarationSyntax);
                if (classDeclarationSyntax == null)
                    continue;
                var actualTypeSymbol = semanticModel.GetDeclaredSymbol(classDeclarationSyntax);
                if (actualTypeSymbol == null)
                    continue;
                if (!SymbolEqualityComparer.Default.Equals(actualTypeSymbol, typeSymbol))
                    return true;
            }

            return false;
        }

        private static IEnumerable<SyntaxNode> GetParents(SyntaxNode? syntaxNode)
        {
            if (syntaxNode == null)
                yield break;

            var currentNode = syntaxNode;
            while (currentNode.Parent != null)
            {
                yield return currentNode.Parent;
                currentNode = currentNode.Parent;
            }
        }

        private static async Task<IEnumerable<INamedTypeSymbol>> GetTypes(Project project)
        {
            var compilation = await project.GetCompilationAsync();
            if (compilation != null)
                return GetTypes(compilation);
            return new List<INamedTypeSymbol>();
        }

        private static IEnumerable<INamedTypeSymbol> GetTypes(Compilation compilation)
        {
            var sourceModule = compilation.SourceModule;
            var globalNamespace = sourceModule.GlobalNamespace;
            var members = globalNamespace.GetMembers();
            var namespaces = members.OfType<INamespaceSymbol>();
            var types = new List<INamedTypeSymbol>();
            foreach (var namespaceSymbol in namespaces)
                CollectTypes(namespaceSymbol, types);
            return types;
        }

        private static void CollectTypes(INamespaceOrTypeSymbol symbol, List<INamedTypeSymbol> types)
        {
            if (symbol is INamedTypeSymbol namedTypeSymbol)
            {
                types.Add(namedTypeSymbol);
                var members = namedTypeSymbol.GetTypeMembers();
                foreach (var member in members)
                    CollectTypes(member, types);
            }

            if (symbol is INamespaceSymbol namespaceSymbol)
            {
                var members = namespaceSymbol.GetMembers();
                foreach (var member in members)
                    CollectTypes(member, types);
            }
        }
    }
}
