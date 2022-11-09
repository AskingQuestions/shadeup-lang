// parse("hello world");

type ShadeupAlert = {
  level: "info" | "warning" | "error";
  message: string;
  location: Number;
};

export default class ShadeupEnvironment {
  files: Map<string, string>;
  alerts: Map<string, ShadeupAlert[]>;
  env: any;
  mod: any;

  constructor() {
    this.files = new Map();
    this.alerts = new Map();
  }

  async load() {
    let exp = await import("../../parser-wasm/pkg/parser_wasm_bg");
    this.mod = exp;
    console.log(exp);
    this.env = exp.make_environment();
    console.log(this.env);
  }

  loaded() {
    if (this.env) {
      return Promise.resolve();
    }
    return new Promise((resolve) => {
      if (this.env) {
        resolve();
      } else {
        const interval = setInterval(() => {
          if (this.env) {
            clearInterval(interval);
            resolve();
          }
        }, 100);
      }
    });
  }

  getAlerts(filename: string) {
    return this.alerts.get(filename) || [];
  }

  evaluate(filename: string) {
    let now = performance.now();
    const content = this.files.get(filename);
    // const ast = this.parse(content);

    // console.log(
    //   ast.alerts.length + " alerts in " + (performance.now() - now) + "ms"
    // );

    // this.alerts.set(filename, ast.alerts);

    this.mod.parse_file(this.env, filename);

    let alerts = this.mod.get_file_alerts(this.env, filename);
    this.alerts.set(filename, alerts);
  }

  updateFile(filename: string, content: string) {
    this.files.set(filename, content);
    this.mod.set_file(this.env, filename, content);
  }
}
